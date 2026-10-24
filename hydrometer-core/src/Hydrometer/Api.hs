{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Types for the server's JSON API, shared with the browser UI.
--
-- Lists are returned as a 'Table' (column names plus rows) and time series
-- as a 'Series' (one time axis plus named columns).  Both are easy to render
-- generically and easy for scripts and AI agents to consume.
module Hydrometer.Api
  ( Table (..)
  , Series (..)
  , NowResponse (..)
  , MachineNow (..)
  , SlotNow (..)
  , QueueNow (..)
  , EvalNow (..)
  , PendingEval (..)
  , Alert (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Options (..), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isUpper, toLower)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

apiOptions :: Options
apiOptions = defaultOptions { fieldLabelModifier = dropPrefix, omitNothingFields = True }
  where
    dropPrefix s = case dropWhile (not . isUpper) s of
      (c:cs) -> toLower c : cs
      [] -> s

data Table = Table
  { tColumns :: ![Text]
  , tRows    :: ![[A.Value]]
  } deriving stock (Show, Eq, Generic)

instance ToJSON Table where toJSON = genericToJSON apiOptions
instance FromJSON Table where parseJSON = genericParseJSON apiOptions

data Series = Series
  { seTime   :: ![Int64]
    -- ^ Bucket start times, epoch ms.
  , seStepMs :: !Int64
  , seSeries :: !(Map Text [Maybe Double])
  } deriving stock (Show, Eq, Generic)

instance ToJSON Series where toJSON = genericToJSON apiOptions
instance FromJSON Series where parseJSON = genericParseJSON apiOptions

data NowResponse = NowResponse
  { nTime     :: !Int64
  , nMachines :: ![MachineNow]
  , nQueues   :: ![QueueNow]
  , nEvals    :: ![EvalNow]
  , nPendingEvals :: ![PendingEval]
    -- ^ In the order the evaluator will start them.
  , nAlerts   :: ![Alert]
  } deriving stock (Show, Eq, Generic)

instance ToJSON NowResponse where toJSON = genericToJSON apiOptions
-- | @pendingEvals@ defaults to empty, so a newer UI still reads an older server.
instance FromJSON NowResponse where
  parseJSON = A.withObject "NowResponse" $ \o ->
    genericParseJSON apiOptions (A.Object (KM.insertWith (\_ old -> old) "pendingEvals" (A.toJSON ([] :: [A.Value])) o))

data MachineNow = MachineNow
  { mName        :: !Text
  , mOs          :: !(Maybe Text)
  , mCpuCount    :: !(Maybe Int)
  , mMemTotal    :: !(Maybe Int64)
  , mLastSample  :: !(Maybe Int64)
    -- ^ Epoch ms of the newest agent sample (Nothing: no agent).
  , mCpuUtil     :: !(Maybe Double)
    -- ^ 0..1 of all cores, over the last sample interval.
  , mLoad1       :: !(Maybe Double)
  , mMemUsed     :: !(Maybe Int64)
  , mMemPressure :: !(Maybe Double)
  , mSwapUsed    :: !(Maybe Int64)
  , mNetRxRate   :: !(Maybe Double)
    -- ^ Bytes per second.
  , mNetTxRate   :: !(Maybe Double)
  , mDiskReadRate  :: !(Maybe Double)
  , mDiskWriteRate :: !(Maybe Double)
  , mOpenFiles   :: !(Maybe Int64)
  , mMaxFiles    :: !(Maybe Int64)
  , mStoreFree   :: !(Maybe Int64)
  , mStoreTotal  :: !(Maybe Int64)
  , mSystems     :: ![Text]
    -- ^ From the queue runner: the systems it builds for.
  , mMaxJobs     :: !(Maybe Int)
  , mDisabled    :: !Bool
  , mClasses     :: !(Map Text Double)
    -- ^ Cores used by non-build process classes (nix-daemon, hydra-builder…).
  , mSlots       :: ![SlotNow]
  , mCpuHistory  :: ![Double]
    -- ^ CPU utilisation (0..1) for the last few minutes, oldest first.
  } deriving stock (Show, Eq, Generic)

instance ToJSON MachineNow where toJSON = genericToJSON apiOptions
instance FromJSON MachineNow where parseJSON = genericParseJSON apiOptions

-- | One build in progress on a machine: a hydra step, an agent run, or
-- (usually) both joined.
data SlotNow = SlotNow
  { slDrvName   :: !Text
  , slDrvPath   :: !(Maybe Text)
  , slKind      :: !(Maybe Text)
    -- ^ @vm@, @native@ or @eval@ (from the agent).
  , slSystem    :: !(Maybe Text)
  , slBuild     :: !(Maybe Int)
  , slStepNr    :: !(Maybe Int)
  , slJob       :: !(Maybe Text)
  , slJobset    :: !(Maybe Text)
  , slProject   :: !(Maybe Text)
  , slStarted   :: !(Maybe Int64)
  , slCores     :: !(Maybe Double)
    -- ^ CPU cores in use over the last interval.
  , slRss       :: !(Maybe Int64)
  , slNetRxRate :: !(Maybe Double)
  , slCpuSeconds :: !(Maybe Double)
    -- ^ CPU seconds so far.
  , slOrphaned  :: !Bool
    -- ^ Hydra's database lists this step as running, but the queue runner
    -- has no such job on the machine: nothing is building it, and its build
    -- waits until the queue runner is restarted.
  } deriving stock (Show, Eq, Generic)

instance ToJSON SlotNow where toJSON = genericToJSON apiOptions
-- | @orphaned@ defaults to false, so a newer UI still reads an older server.
instance FromJSON SlotNow where
  parseJSON = A.withObject "SlotNow" $ \o ->
    genericParseJSON apiOptions (A.Object (KM.insertWith (\_ old -> old) "orphaned" (A.Bool False) o))

data QueueNow = QueueNow
  { qSystem   :: !Text
  , qRunnable :: !Int
    -- ^ Steps whose dependencies are built, waiting for a machine.
  , qOldestRunnableMs :: !(Maybe Int64)
    -- ^ How long the longest-waiting runnable step has waited.
  } deriving stock (Show, Eq, Generic)

instance ToJSON QueueNow where toJSON = genericToJSON apiOptions
instance FromJSON QueueNow where parseJSON = genericParseJSON apiOptions

data EvalNow = EvalNow
  { eProject  :: !Text
  , eJobset   :: !Text
  , eStarted  :: !(Maybe Int64)
  , eCores    :: !(Maybe Double)
  , eRss      :: !(Maybe Int64)
  , eLastEval :: !(Maybe Int)
    -- ^ The id of the jobset's previous evaluation.
  } deriving stock (Show, Eq, Generic)

instance ToJSON EvalNow where toJSON = genericToJSON apiOptions
instance FromJSON EvalNow where parseJSON = genericParseJSON apiOptions

-- | A jobset the evaluator will evaluate when a slot is free.
data PendingEval = PendingEval
  { peProject   :: !Text
  , peJobset    :: !Text
  , peRequested :: !(Maybe Int64)
    -- ^ When an evaluation was requested (epoch ms); Nothing when it is due
    -- because its check interval elapsed.
  , peLastChecked :: !(Maybe Int64)
  , peLastEval  :: !(Maybe Int)
  , peLastEvalSeconds :: !(Maybe Int)
    -- ^ How long the jobset's latest evaluation took (checkout + eval).
  } deriving stock (Show, Eq, Generic)

instance ToJSON PendingEval where toJSON = genericToJSON apiOptions
instance FromJSON PendingEval where parseJSON = genericParseJSON apiOptions

data Alert = Alert
  { alLevel   :: !Text
    -- ^ @warning@ or @critical@.
  , alMessage :: !Text
  } deriving stock (Show, Eq, Generic)

instance ToJSON Alert where toJSON = genericToJSON apiOptions
instance FromJSON Alert where parseJSON = genericParseJSON apiOptions
