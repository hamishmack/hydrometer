-- | Pull samples from the agents into the database.
--
-- One thread per agent.  The cursor (agent start time + last sequence
-- number) is stored in @hydrometer.machines@, so a server restart resumes
-- where it left off and nothing still in the agent's ring buffer is lost.
module Hydrometer.Server.Ingest
  ( AgentState (..)
  , runAgentPoller
  , ingest
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Aeson qualified as A
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Network.HTTP.Client
import System.IO (hPutStrLn, stderr)

import Hydrometer.Server.Config (AgentConfig (..))
import Hydrometer.Server.Db
import Hydrometer.Types

-- | What the server knows about an agent right now (for /api/now).
data AgentState = AgentState
  { asInfo     :: !(Maybe AgentInfo)
  , asLatest   :: !(Maybe Sample)
  , asRecent   :: ![HostStats]
    -- ^ The last few minutes of host samples, newest first.
  , asError    :: !(Maybe Text)
  , asPolled   :: !(Maybe UTCTime)
  }

runAgentPoller :: Db -> Manager -> Int -> TVar (Map Text AgentState) -> AgentConfig -> IO ()
runAgentPoller d mgr pollSec states ac = do
  atomically $ modifyTVar' states (M.insertWith (\_ old -> old) (name ac) (AgentState Nothing Nothing [] Nothing Nothing))
  cursor0 <- withConn d $ \c -> do
    rs <- query c "SELECT agent_start, last_seq FROM hydrometer.machines WHERE name = ?" (Only (name ac))
    pure $ case rs of
      [(Just st, Just s)] -> Just (epochMs st, s)
      _ -> Nothing
  let loop cursor = do
        r <- try (pollOnce cursor) :: IO (Either SomeException (Maybe (Int64, Int64), Bool))
        (cursor', more) <- case r of
          Right x -> pure x
          Left e -> do
            now <- getCurrentTime
            atomically $ modifyTVar' states $ M.adjust (\s -> s { asError = Just (T.pack (show e)), asPolled = Just now }) (name ac)
            hPutStrLn stderr ("agent " <> T.unpack (name ac) <> ": " <> show e)
            pure (cursor, False)
        -- A full batch means we are catching up: go again at once.
        if more then pure () else threadDelay (pollSec * 1000000)
        loop cursor'
  loop cursor0
  where
    pollOnce cursor = do
      let since = maybe 0 snd cursor
      req <- parseRequest (T.unpack (url ac) <> "/samples?since=" <> show since)
      resp <- httpLbs req { responseTimeout = responseTimeoutMicro 20000000 } mgr
      sr <- either fail pure (A.eitherDecode (responseBody resp))
      let info = srAgent sr
          restarted = case cursor of
            Just (st, _) -> st /= aStartTime info
            Nothing -> False
      -- The agent restarted: its sequence numbers start again at 1, so the
      -- samples we asked for were relative to the old process.  Ask again.
      sr' <- if restarted && since > 0
        then do
          req0 <- parseRequest (T.unpack (url ac) <> "/samples?since=0")
          r0 <- httpLbs req0 { responseTimeout = responseTimeoutMicro 20000000 } mgr
          either fail pure (A.eitherDecode (responseBody r0))
        else pure sr
      ingest d (name ac) (url ac) sr'
      now <- getCurrentTime
      let samples = srSamples sr'
      atomically $ modifyTVar' states $ M.adjust (\s -> s
        { asInfo = Just (srAgent sr')
        , asLatest = maybe (asLatest s) Just (lastMay samples)
        , asRecent = take 72 (reverse (map sHost samples) ++ asRecent s)
        , asError = Nothing
        , asPolled = Just now }) (name ac)
      let cursor' = case lastMay samples of
            Just s -> Just (aStartTime (srAgent sr'), sSeq s)
            Nothing -> if restarted then Just (aStartTime info, 0) else cursor
      pure (cursor', length samples >= 240)
    lastMay xs = if null xs then Nothing else Just (last xs)

-- | Write one batch from an agent in a single transaction.
ingest :: Db -> Text -> Text -> SamplesResponse -> IO ()
ingest d machine agentUrl sr = withTx d $ \c -> do
  let info = srAgent sr
  void $ execute c
    "INSERT INTO hydrometer.machines (name, os, os_version, cpu_count, mem_total, agent_url, agent_version, agent_start, boot_time, builders) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (name) DO UPDATE SET os = excluded.os, os_version = excluded.os_version, cpu_count = excluded.cpu_count, \
    \mem_total = excluded.mem_total, agent_url = excluded.agent_url, agent_version = excluded.agent_version, \
    \agent_start = excluded.agent_start, boot_time = excluded.boot_time, \
    \builders = CASE WHEN excluded.builders = '[]'::jsonb THEN machines.builders ELSE excluded.builders END"
    ( machine, aOs info, aOsVersion info, aCpuCount info, aMemTotal info, agentUrl, aAgentVersion info
    , fromEpochMs (aStartTime info), fromEpochMs (aBootTime info), A.toJSON (aBuilders info) )
  forM_ (srSamples sr) $ \s -> do
    let t = fromEpochMs (sTime s)
        h = sHost s
    void $ execute c
      "INSERT INTO hydrometer.host_samples VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
      ( (machine, t, hIntervalMs h, hCpuUserMs h, hCpuSystemMs h, hCpuIdleMs h, hLoad1 h, hLoad5 h, hLoad15 h, hMemTotal h)
        :. (hMemUsed h, hMemCompressed h, hMemPressure h, hCpuPressure h, hIoPressure h, hSwapUsed h, hSwapTotal h, hPageouts h)
        :. (hNetRxBytes h, hNetTxBytes h, hDiskReadBytes h, hDiskWriteBytes h, hOpenFiles h, hMaxFiles h, hStoreFree h, hStoreTotal h) )
    forM_ (sRuns s) $ \r -> do
      [Only runId] <- query c
        "INSERT INTO hydrometer.runs AS r (machine, run_key, kind, name, outputs, system, started, first_seen, last_seen, \
        \  cpu_ms, peak_rss, peak_footprint, disk_read_bytes, disk_write_bytes, net_rx_bytes, net_tx_bytes, max_procs, samples) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1) \
        \ON CONFLICT (machine, run_key) DO UPDATE SET \
        \  kind = excluded.kind, \
        \  name = CASE WHEN excluded.outputs <> '{}' THEN excluded.name ELSE r.name END, \
        \  outputs = CASE WHEN excluded.outputs <> '{}' THEN excluded.outputs ELSE r.outputs END, \
        \  system = coalesce(excluded.system, r.system), \
        \  first_seen = least(r.first_seen, excluded.first_seen), \
        \  last_seen = greatest(r.last_seen, excluded.last_seen), \
        \  cpu_ms = r.cpu_ms + excluded.cpu_ms, \
        \  peak_rss = greatest(r.peak_rss, excluded.peak_rss), \
        \  peak_footprint = greatest(r.peak_footprint, excluded.peak_footprint), \
        \  disk_read_bytes = r.disk_read_bytes + excluded.disk_read_bytes, \
        \  disk_write_bytes = r.disk_write_bytes + excluded.disk_write_bytes, \
        \  net_rx_bytes = r.net_rx_bytes + excluded.net_rx_bytes, \
        \  net_tx_bytes = r.net_tx_bytes + excluded.net_tx_bytes, \
        \  max_procs = greatest(r.max_procs, excluded.max_procs), \
        \  samples = r.samples + 1, \
        \  resolution = CASE WHEN r.resolution = 'unmatched' THEN NULL ELSE r.resolution END \
        \RETURNING id"
        ( (machine, rKey r, runKindText (rKind r), rName r, PGArray (rOutputs r), rSystem r, fromEpochMs (rStartTime r), t, t)
          :. (rCpuMs r, rRss r, rFootprint r, rDiskReadBytes r, rDiskWriteBytes r, rNetRxBytes r, rNetTxBytes r, rProcs r) )
      void $ execute c
        "INSERT INTO hydrometer.run_samples VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
        ( runId :: Int64, t, rCpuMs r, rRss r, rFootprint r, rDiskReadBytes r, rDiskWriteBytes r
        , rNetRxBytes r, rNetTxBytes r, rProcs r )
    void $ executeMany c
      "INSERT INTO hydrometer.class_samples VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
      [ ( machine, t, cClass k, cCpuMs k, cRss k, cDiskReadBytes k, cDiskWriteBytes k
        , cNetRxBytes k, cNetTxBytes k, cProcs k )
      | k <- sClasses s ]
  case listToMaybe (reverse (srSamples sr)) of
    Just s -> void $ execute c
      "UPDATE hydrometer.machines SET last_seq = ?, last_sample = ? WHERE name = ?"
      (sSeq s, fromEpochMs (sTime s), machine)
    Nothing -> when False (pure ())
