-- | Poll the hydra queue runner's REST API (@/status/machines@,
-- @/status/queues@) and record what it believes.
module Hydrometer.Server.QueueRunner
  ( QueueState (..)
  , QrMachine (..)
  , emptyQueueState
  , runQueuePoller
  , parseMachines
  , parseQueues
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as K
import Data.Aeson.Types qualified as A
import Data.Int (Int64)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Network.HTTP.Client
import System.IO (hPutStrLn, stderr)

import Hydrometer.Server.Db

data QrMachine = QrMachine
  { qmName         :: !Text
  , qmSystems      :: ![Text]
  , qmMaxJobs      :: !Int
  , qmCurrentJobs  :: !Int
  , qmDisabled     :: !Bool
  , qmConsecutiveFailures :: !Int
  , qmLoad1        :: !(Maybe Double)
  , qmMemUsage     :: !(Maybe Int64)
  , qmStepsDone    :: !(Maybe Int64)
  , qmAvgImportMs  :: !(Maybe Int64)
  , qmAvgBuildMs   :: !(Maybe Int64)
  , qmUploading    :: !(Maybe Int)
  , qmDownloading  :: !(Maybe Int)
  } deriving stock (Show)

data QueueState = QueueState
  { qsMachines :: ![QrMachine]
  , qsQueues   :: ![(Text, Int, Maybe Int64)]
    -- ^ (system, runnable steps waiting for a machine, oldest wait ms)
  , qsPolled   :: !(Maybe UTCTime)
  , qsError    :: !(Maybe Text)
  }

emptyQueueState :: QueueState
emptyQueueState = QueueState [] [] Nothing Nothing

parseMachines :: A.Value -> [QrMachine]
parseMachines v = maybe [] id $ flip A.parseMaybe v $ A.withObject "status" $ \o -> do
  ms <- o .: "machines" :: A.Parser A.Object
  pure $ mapMaybe (\(k, m) -> A.parseMaybe (machine (K.toText k)) m) (KM.toList ms)
  where
    machine key = A.withObject "machine" $ \m -> do
      stats <- m .:? "stats" .!= KM.empty
      let stat :: A.FromJSON a => Text -> A.Parser (Maybe a)
          stat k = A.explicitParseFieldMaybe A.parseJSON stats (K.fromText k)
      disabledUntil <- stat "disabledUntil" :: A.Parser (Maybe Int64)
      QrMachine
        <$> (m .:? "hostname" .!= key)
        <*> m .:? "systems" .!= []
        <*> m .:? "maxJobs" .!= 1
        <*> (maybe 0 id <$> stat "currentJobs")
        <*> pure (maybe False (> 0) disabledUntil)
        <*> (maybe 0 id <$> stat "consecutiveFailures")
        <*> stat "load1"
        <*> stat "memUsage"
        <*> stat "nrStepsDone"
        <*> stat "avgStepImportTimeMs"
        <*> stat "avgStepBuildTimeMs"
        <*> stat "currentUploadingPathCount"
        <*> stat "currentDownloadingPathCount"

-- | Steps that are runnable (all dependencies built) and not yet
-- scheduled on a machine, per system.
parseQueues :: UTCTime -> A.Value -> [(Text, Int, Maybe Int64)]
parseQueues now v = maybe [] id $ flip A.parseMaybe v $ A.withObject "queues" $ \o -> do
  qs <- o .: "queues" :: A.Parser (KM.KeyMap [A.Value])
  pure [ (K.toText k, length waiting, if null waits then Nothing else Just (maximum waits))
       | (k, steps) <- KM.toList qs
       , let waiting = filter isWaiting steps
             waits = catMaybes (map waitMs waiting) ]
  where
    field :: A.FromJSON a => Text -> A.Value -> Maybe a
    field k = A.parseMaybe (A.withObject "step" (\s -> s .: K.fromText k))
    isWaiting s = field "runnable" s == Just True && field "alreadyScheduled" s /= Just True
    waitMs s = do
      t <- field "runnableSince" s
      since <- iso8601ParseM (T.unpack t) :: Maybe UTCTime
      pure (round (realToFrac (diffUTCTime now since) * 1000 :: Double))

runQueuePoller :: Db -> Manager -> Text -> TVar QueueState -> IO ()
runQueuePoller d mgr base st = forever $ do
  r <- try pollOnce :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left e -> do
      hPutStrLn stderr ("queue runner: " <> show e)
      atomically $ modifyTVar' st (\s -> s { qsError = Just (T.pack (show e)) })
  threadDelay 15000000
  where
    get path = do
      req <- parseRequest (T.unpack base <> path)
      resp <- httpLbs req { responseTimeout = responseTimeoutMicro 20000000 } mgr
      either fail pure (A.eitherDecode (responseBody resp))
    pollOnce = do
      mv <- get "/status/machines"
      qv <- get "/status/queues"
      now <- getCurrentTime
      let ms = parseMachines mv
          qs = parseQueues now qv
      atomically $ writeTVar st (QueueState ms qs (Just now) Nothing)
      withTx d $ \c -> do
        void $ executeMany c
          "INSERT INTO hydrometer.machine_status_samples VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
          [ ( (now, qmName m, PGArray (qmSystems m), qmMaxJobs m, qmCurrentJobs m, qmDisabled m, qmConsecutiveFailures m)
              :. (qmLoad1 m, qmMemUsage m, qmStepsDone m, qmAvgImportMs m, qmAvgBuildMs m, qmUploading m, qmDownloading m) )
          | m <- ms ]
        void $ executeMany c
          "INSERT INTO hydrometer.queue_samples VALUES (?,?,?,?) ON CONFLICT DO NOTHING"
          [ (now, sys, n, w) | (sys, n, w) <- qs ]
