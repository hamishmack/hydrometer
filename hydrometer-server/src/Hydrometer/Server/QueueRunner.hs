-- | Poll the hydra queue runner's REST API (@/status/machines@,
-- @/status/queues@) and record what it believes.
module Hydrometer.Server.QueueRunner
  ( QueueState (..)
  , QrMachine (..)
  , QrRegistration (..)
  , QrStep (..)
  , emptyQueueState
  , queueAsOf
  , stepOrphaned
  , stepUploading
  , runQueuePoller
  , fetchQueueState
  , parseMachines
  , parseRegistrations
  , mergeByHost
  , parseQueues
  , parseQueueSteps
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
import Data.List (foldl1', nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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
  , qmJobs         :: ![Text]
    -- ^ The steps it is running, as .drv basenames (no store directory).
  , qmFreedJobs    :: ![Text]
    -- ^ Steps it has built and freed the slot of, whose outputs it is still
    -- uploading (queue runner's freedJobs; empty from runners without it).
  } deriving stock (Show)

-- | One builder registration, with what decides whether it gets work.
data QrRegistration = QrRegistration
  { qrHost        :: !Text
  , qrSystems     :: ![Text]
  , qrSupported   :: ![Text]
  , qrMandatory   :: ![Text]
  , qrMaxJobs     :: !Int
  , qrCurrentJobs :: !Int
  , qrHasCapacity :: !Bool
  , qrDisabled    :: !Bool
  , qrStoreFree   :: !(Maybe Double)
    -- ^ Percent; the builder refuses work below 'qrStoreMin'.
  , qrStoreMin    :: !(Maybe Double)
  , qrBuildDirFree :: !(Maybe Double)
  , qrBuildDirMin :: !(Maybe Double)
  , qrLoad1       :: !(Maybe Double)
  , qrLoad1Max    :: !(Maybe Double)
  } deriving stock (Show)

-- | An entry in the queue runner's per-system queues.
data QrStep = QrStep
  { qeDrv       :: !Text
    -- ^ .drv basename (no store directory).
  , qeSystem    :: !Text
  , qeScheduled :: !Bool
    -- ^ Handed to a builder.
  , qeWaiting   :: !Bool
    -- ^ Runnable and not handed to a builder: waiting for a machine.
  , qeSince     :: !(Maybe UTCTime)
    -- ^ When it became runnable.
  } deriving stock (Show)

data QueueState = QueueState
  { qsMachines :: ![QrMachine]
    -- ^ Merged per host.
  , qsRegistrations :: ![QrRegistration]
  , qsSteps    :: ![QrStep]
  , qsQueues   :: ![(Text, Int, Maybe Int64)]
    -- ^ (system, runnable steps waiting for a machine, oldest wait ms)
  , qsPolled   :: !(Maybe UTCTime)
  , qsError    :: !(Maybe Text)
  }

emptyQueueState :: QueueState
emptyQueueState = QueueState [] [] [] [] Nothing Nothing

-- | When the queue runner's job lists can be trusted to judge hydra's busy
-- steps against: the time (epoch ms) of a successful poll in the last
-- minute.
queueAsOf :: Int64 -> QueueState -> Maybe Int64
queueAsOf now qs = case (qsPolled qs, qsError qs) of
  (Just t, Nothing) | let ms = round (utcTimeToPOSIXSeconds t * 1000), now - ms < 60000 -> Just ms
  _ -> Nothing

-- | A step hydra lists as running on @host@ that the queue runner has no
-- job for there.  Only steps that started well before the poll count, since
-- a step dispatched after it is not in its list yet.
stepOrphaned :: Int64 -> QueueState -> Text -> Text -> Maybe Int64 -> Bool
stepOrphaned now qs host drvPath started = case queueAsOf now qs of
  Just asOf -> T.takeWhileEnd (/= '/') drvPath `notElem` jobs && maybe False (< asOf - 120000) started
  Nothing -> False
  where
    jobs = concat [ qmJobs m ++ qmFreedJobs m | m <- qsMachines qs, qmName m == host ]

-- | The step is built and its builder is uploading the outputs: the slot is
-- free, but hydra lists the step as busy until the upload completes.
stepUploading :: QueueState -> Text -> Text -> Bool
stepUploading qs host drvPath =
  T.takeWhileEnd (/= '/') drvPath `elem` concat [ qmFreedJobs m | m <- qsMachines qs, qmName m == host ]

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
        <*> (mapMaybe (A.parseMaybe A.parseJSON) <$> m .:? "jobs" .!= ([] :: [A.Value]))
        <*> (mapMaybe (A.parseMaybe A.parseJSON) <$> m .:? "freedJobs" .!= ([] :: [A.Value]))

parseRegistrations :: A.Value -> [QrRegistration]
parseRegistrations v = maybe [] id $ flip A.parseMaybe v $ A.withObject "status" $ \o -> do
  ms <- o .: "machines" :: A.Parser A.Object
  pure $ mapMaybe (\(k, m) -> A.parseMaybe (registration (K.toText k)) m) (KM.toList ms)
  where
    registration key = A.withObject "machine" $ \m -> do
      stats <- m .:? "stats" .!= KM.empty
      let stat :: A.FromJSON a => Text -> A.Parser (Maybe a)
          stat k = A.explicitParseFieldMaybe A.parseJSON stats (K.fromText k)
      disabledUntil <- stat "disabledUntil" :: A.Parser (Maybe Int64)
      QrRegistration
        <$> (m .:? "hostname" .!= key)
        <*> m .:? "systems" .!= []
        <*> m .:? "supportedFeatures" .!= []
        <*> m .:? "mandatoryFeatures" .!= []
        <*> m .:? "maxJobs" .!= 1
        <*> (maybe 0 id <$> stat "currentJobs")
        <*> m .:? "hasCapacity" .!= True
        <*> pure (maybe False (> 0) disabledUntil)
        <*> stat "storeFreePercent"
        <*> m .:? "storeAvailThreshold"
        <*> stat "buildDirFreePercent"
        <*> m .:? "buildDirAvailThreshold"
        <*> stat "load1"
        <*> m .:? "load1Threshold"

-- | One entry per host.  The queue runner lists each builder registration
-- separately, and a Mac runs two (a darwin builder and a linux one), but
-- hydra's buildsteps.machine (and everything in hydrometer) is per host.
mergeByHost :: [QrMachine] -> [QrMachine]
mergeByHost ms = map (foldl1' merge) (M.elems (M.fromListWith (flip (++)) [ (qmName m, [m]) | m <- ms ]))
  where
    merge a b = a
      { qmSystems = nub (qmSystems a ++ qmSystems b)
      , qmMaxJobs = qmMaxJobs a + qmMaxJobs b
      , qmCurrentJobs = qmCurrentJobs a + qmCurrentJobs b
        -- The host is disabled only when every builder on it is.
      , qmDisabled = qmDisabled a && qmDisabled b
      , qmConsecutiveFailures = max (qmConsecutiveFailures a) (qmConsecutiveFailures b)
        -- load1 and memory are the host's own (each builder reports them);
        -- the step-time averages stay those of the first registration.
      , qmStepsDone = (+) <$> qmStepsDone a <*> qmStepsDone b
      , qmUploading = (+) <$> qmUploading a <*> qmUploading b
      , qmDownloading = (+) <$> qmDownloading a <*> qmDownloading b
      , qmJobs = qmJobs a ++ qmJobs b
      , qmFreedJobs = qmFreedJobs a ++ qmFreedJobs b
      }

-- | Every queue entry.
parseQueueSteps :: A.Value -> [QrStep]
parseQueueSteps v = maybe [] id $ flip A.parseMaybe v $ A.withObject "queues" $ \o -> do
  qs <- o .: "queues" :: A.Parser (KM.KeyMap [A.Value])
  pure [ QrStep d (K.toText k) (field "alreadyScheduled" s == Just True) (isWaiting s)
                (field "runnableSince" s >>= iso8601ParseM . T.unpack)
       | (k, steps) <- KM.toList qs, s <- steps, Just d <- [field "drvPath" s] ]
  where
    field :: A.FromJSON a => Text -> A.Value -> Maybe a
    field k = A.parseMaybe (A.withObject "step" (\s -> s .: K.fromText k))
    isWaiting s = field "runnable" s == Just True && field "alreadyScheduled" s /= Just True
                  && field "finished" s /= Just True && field "cancelled" s /= Just True

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
    -- The queue keeps entries for steps that have since finished or been
    -- cancelled (still marked runnable); those are not waiting for anything.
    isWaiting s = field "runnable" s == Just True && field "alreadyScheduled" s /= Just True
                  && field "finished" s /= Just True && field "cancelled" s /= Just True
    waitMs s = do
      t <- field "runnableSince" s
      since <- iso8601ParseM (T.unpack t) :: Maybe UTCTime
      pure (round (realToFrac (diffUTCTime now since) * 1000 :: Double))

-- | Poll the queue runner once, without recording anything.
fetchQueueState :: Manager -> Text -> IO QueueState
fetchQueueState mgr base = do
  let get path = do
        req <- parseRequest (T.unpack base <> path)
        resp <- httpLbs req { responseTimeout = responseTimeoutMicro 20000000 } mgr
        either fail pure (A.eitherDecode (responseBody resp))
  mv <- get "/status/machines"
  qv <- get "/status/queues"
  now <- getCurrentTime
  pure (QueueState (mergeByHost (parseMachines mv)) (parseRegistrations mv) (parseQueueSteps qv) (parseQueues now qv) (Just now) Nothing)

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
      let ms = mergeByHost (parseMachines mv)
          qs = parseQueues now qv
      atomically $ writeTVar st (QueueState ms (parseRegistrations mv) (parseQueueSteps qv) qs (Just now) Nothing)
      withTx d $ \c -> do
        void $ executeMany c
          "INSERT INTO hydrometer.machine_status_samples VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
          [ ( (now, qmName m, PGArray (qmSystems m), qmMaxJobs m, qmCurrentJobs m, qmDisabled m, qmConsecutiveFailures m)
              :. (qmLoad1 m, qmMemUsage m, qmStepsDone m, qmAvgImportMs m, qmAvgBuildMs m, qmUploading m, qmDownloading m) )
          | m <- ms ]
        void $ executeMany c
          "INSERT INTO hydrometer.queue_samples VALUES (?,?,?,?) ON CONFLICT DO NOTHING"
          [ (now, sys, n, w) | (sys, n, w) <- qs ]
