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
  , EvalBlockers (..)
  , IfdNow (..)
  , IfdWorker (..)
  , IfdWait (..)
  , IfdBuilding (..)
  , EvalIfd (..)
  , CacheNow (..)
  , LogPattern (..)
  , Blocker (..)
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
  , nEvalSlots :: !(Maybe Int)
    -- ^ How many evaluations run at once (hydra-evaluator's
    -- max_concurrent_evals, as configured).
  , nCache     :: !(Maybe CacheNow)
  , nLogPatterns :: ![LogPattern]
    -- ^ Errors and warnings in hydra's journal in the last 15 minutes.
  , nAlerts   :: ![Alert]
  } deriving stock (Show, Eq, Generic)

instance ToJSON NowResponse where toJSON = genericToJSON apiOptions
-- | List fields added since the first release default to empty, so a newer
-- UI still reads an older server.
instance FromJSON NowResponse where
  parseJSON = A.withObject "NowResponse" $ \o ->
    genericParseJSON apiOptions $ A.Object $
      foldr (\k -> KM.insertWith (\_ old -> old) k (A.toJSON ([] :: [A.Value]))) o ["pendingEvals", "logPatterns"]

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
  , mFreeVnodes  :: !(Maybe Int64)
  , mMaxVnodes   :: !(Maybe Int64)
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
  , slPrefetched :: !Bool
    -- ^ The queue runner sent this step ahead of a free slot (hydra-builder's
    -- prefetch_factor): its inputs are imported and it is waiting for one of
    -- the machine's max_jobs slots, so it uses no build slot yet.
  , slUploading :: !Bool
    -- ^ Built; the builder freed its slot and is uploading the outputs to the
    -- queue runner.  Hydra lists the step as busy until the upload completes.
  } deriving stock (Show, Eq, Generic)

instance ToJSON SlotNow where toJSON = genericToJSON apiOptions
-- | @orphaned@, @prefetched@ and @uploading@ default to false, so a newer UI
-- still reads an older server.
instance FromJSON SlotNow where
  parseJSON = A.withObject "SlotNow" $ \o ->
    genericParseJSON apiOptions
      (A.Object (foldr (\k -> KM.insertWith (\_ old -> old) k (A.Bool False)) o ["orphaned", "prefetched", "uploading"]))

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
  , eLastEvalSeconds :: !(Maybe Int)
    -- ^ How long that evaluation took (checkout + eval).
  , eTypicalSeconds :: !(Maybe Int)
    -- ^ Median duration of the jobset's last five evaluations.
  , eGuideJobset :: !(Maybe Text)
    -- ^ Set when the jobset has never been evaluated: the jobset (the
    -- project's @master@) the three fields above and 'eGuideJobs' describe.
  , eGuideJobs :: !(Maybe Int)
    -- ^ Builds in the latest of those evaluations that had any, a guide to
    -- how many jobs this one will have.
  , ePhase :: !(Maybe Text)
    -- ^ @fetching@, @evaluating@ or @adding@ (adding the builds to hydra's
    -- database), from hydra-eval-jobset's log.
  , eJobsEvaluated :: !(Maybe Int)
    -- ^ Jobs evaluated so far.  None is built, or even in the database,
    -- until the evaluation finishes.
  , eJobsFailed :: !(Maybe Int)
    -- ^ Of those, how many failed to evaluate.
  , eJobsChecked :: !(Maybe Int)
    -- ^ While adding: jobs checked against existing builds so far.
  , eFlake :: !(Maybe Text)
    -- ^ The flake being evaluated, e.g. @github:owner/repo/rev@.
  , eIfd :: !(Maybe IfdNow)
    -- ^ What its nix-eval-jobs workers are doing, from hydra-evaluator's
    -- trace lines (absent without them).
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
  , peTypicalSeconds :: !(Maybe Int)
    -- ^ Median duration of the jobset's last five evaluations.
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

-- | What is holding an evaluation's unfinished builds (@/api/eval/{id}/blockers@).
data EvalBlockers = EvalBlockers
  { ebUnfinished :: !Int
  , ebBuilding   :: !Int
    -- ^ Builds whose own step is running (or handed to a builder).
  , ebOrphaned   :: !Int
    -- ^ Builds whose own step hydra lists as running but no builder has.
  , ebWaiting    :: !Int
    -- ^ Builds whose own step is runnable, waiting for a machine.
  , ebBlocked    :: !Int
    -- ^ Builds waiting on one of the blockers below.
  , ebOther      :: !Int
    -- ^ None of these: not yet in the queue runner, or a dependency failed.
  , ebBlockers   :: ![Blocker]
    -- ^ The steps everything waits on, the most builds held first.
  , ebComputedAt :: !Int64
  , ebDurationMs :: !Int
  , ebDrvsRead   :: !Int
  , ebTruncated  :: !Bool
    -- ^ The dependency walk stopped early (too many derivations).
  } deriving stock (Show, Eq, Generic)

instance ToJSON EvalBlockers where toJSON = genericToJSON apiOptions
instance FromJSON EvalBlockers where parseJSON = genericParseJSON apiOptions

-- | A step that unfinished builds are waiting on.
data Blocker = Blocker
  { blDrvPath  :: !Text
  , blName     :: !Text
  , blState    :: !Text
    -- ^ @building@, @scheduled@, @orphaned@ or @waiting@.
  , blMachine  :: !(Maybe Text)
  , blSystem   :: !(Maybe Text)
  , blFeatures :: ![Text]
  , blSince    :: !(Maybe Int64)
    -- ^ Started (building, orphaned) or runnable since (waiting), epoch ms.
  , blReason   :: !(Maybe Text)
    -- ^ For a waiting step, why no builder takes it, when that is known.
  , blBuilds   :: !Int
    -- ^ How many of the evaluation's unfinished builds wait on it.
  } deriving stock (Show, Eq, Generic)

instance ToJSON Blocker where toJSON = genericToJSON apiOptions
instance FromJSON Blocker where parseJSON = genericParseJSON apiOptions

-- | Whether finished outputs reach the binary cache.
data CacheNow = CacheNow
  { cnUrl      :: !Text
  , cnChecked  :: !Int64
  , cnSampled  :: !Int
    -- ^ Outputs of steps that finished 30-120 minutes before the check.
  , cnPresent  :: !Int
    -- ^ Of those, how many the cache has a narinfo for.
  , cnMissing  :: ![Text]
  , cnQueued   :: !(Maybe Int)
    -- ^ Paths in hydra-attic-bridge's queue.
  , cnRetrying :: !(Maybe Int)
    -- ^ Of those, how many have failed at least once.
  , cnStaleNew :: !(Maybe Int)
    -- ^ Of those, how many were queued over an hour ago and never tried.
  , cnError    :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)

instance ToJSON CacheNow where toJSON = genericToJSON apiOptions
instance FromJSON CacheNow where parseJSON = genericParseJSON apiOptions

-- | A kind of error or warning line in a systemd unit's journal.
data LogPattern = LogPattern
  { lpUnit    :: !Text
  , lpLevel   :: !Text
    -- ^ @error@ or @warning@.
  , lpPattern :: !Text
  , lpCount   :: !Int
  , lpExample :: !Text
  , lpLast    :: !Int64
  } deriving stock (Show, Eq, Generic)

instance ToJSON LogPattern where toJSON = genericToJSON apiOptions
instance FromJSON LogPattern where parseJSON = genericParseJSON apiOptions

-- | A running evaluation's import-from-derivation (IFD) builds and
-- nix-eval-jobs workers, from the trace lines in hydra-evaluator's journal.
-- Each worker blocks on an IFD until it is built, so a slow one can hold
-- every worker.
data IfdNow = IfdNow
  { inRun          :: !Int
    -- ^ hydrometer.evaluations.id (see @/api/ifd/{run}@).
  , inWorkers      :: ![IfdWorker]
    -- ^ Live workers: started, not exited, and active in the last 10 minutes
    -- or waiting on an IFD.
  , inWaits        :: ![IfdWait]
    -- ^ The IFDs workers are waiting on now, the most workers first.
  , inBuilding     :: ![IfdBuilding]
    -- ^ Builds the evaluation started (an IFD or one of its dependencies)
    -- whose outputs are not in the store yet, newest first.
  , inIfds         :: !Int
    -- ^ IFDs finished so far.
  , inWaitMs       :: !Int64
    -- ^ Worker-milliseconds spent waiting on them.
  , inAllWaitingMs :: !Int64
    -- ^ How long every live worker has been waiting on an IFD at once.
  } deriving stock (Show, Eq, Generic)

instance ToJSON IfdNow where toJSON = genericToJSON apiOptions
instance FromJSON IfdNow where parseJSON = genericParseJSON apiOptions

-- | One nix-eval-jobs worker process.
data IfdWorker = IfdWorker
  { wkPid          :: !Int
  , wkSince        :: !(Maybe Int64)
    -- ^ When it was spawned (epoch ms), if its start was seen.
  , wkJobs         :: !Int
    -- ^ Attributes it has evaluated.
  , wkLastActive   :: !Int64
    -- ^ Its last job or IFD line.
  , wkRssMb        :: !(Maybe Int)
  , wkWaitingOn    :: !(Maybe Text)
    -- ^ The .drv of the IFD it is blocked on, if any.
  , wkWaitingSince :: !(Maybe Int64)
  } deriving stock (Show, Eq, Generic)

instance ToJSON IfdWorker where toJSON = genericToJSON apiOptions
instance FromJSON IfdWorker where parseJSON = genericParseJSON apiOptions

-- | An IFD that workers are blocked on.
data IfdWait = IfdWait
  { wtDrv           :: !Text
  , wtWorkers       :: !Int
    -- ^ How many workers wait on it.
  , wtSince         :: !Int64
    -- ^ When the first of them started waiting.
  , wtHost          :: !(Maybe Text)
    -- ^ Where its latest build started, when the evaluation logged one.
    -- Often a dependency is what is building: see 'inBuilding'.
  , wtBuildingSince :: !(Maybe Int64)
  } deriving stock (Show, Eq, Generic)

instance ToJSON IfdWait where toJSON = genericToJSON apiOptions
instance FromJSON IfdWait where parseJSON = genericParseJSON apiOptions

-- | A build an evaluation started (@building '...' on '...'@).
data IfdBuilding = IfdBuilding
  { bgDrv   :: !Text
  , bgHost  :: !(Maybe Text)
    -- ^ The remote builder; Nothing for a local build.
  , bgSince :: !Int64
  } deriving stock (Show, Eq, Generic)

instance ToJSON IfdBuilding where toJSON = genericToJSON apiOptions
instance FromJSON IfdBuilding where parseJSON = genericParseJSON apiOptions

-- | One evaluation's IFDs and workers (@/api/ifd/{run}@,
-- @/api/eval/{id}/ifd@).  Worker-milliseconds sum over workers, so with 8
-- workers an hour of evaluation is 8 worker-hours.
data EvalIfd = EvalIfd
  { eiRun          :: !Int
  , eiPid          :: !Int
    -- ^ hydra-eval-jobset's pid.
  , eiProject      :: !Text
  , eiJobset       :: !Text
  , eiEvalId       :: !(Maybe Int)
    -- ^ hydra's jobsetevals.id, once hydra has written it.
  , eiStarted      :: !Int64
    -- ^ The first trace line (epoch ms).
  , eiLastSeen     :: !Int64
    -- ^ The last trace line; now, while it runs.
  , eiRunning      :: !Bool
  , eiIfds         :: !Int
  , eiByStatus     :: !(Map Text Int)
    -- ^ IFDs by how they ended: built, substituted, already-valid (often
    -- after waiting on another worker's build), permanent-failure, …
  , eiWaitMs       :: !Int64
    -- ^ Worker-milliseconds blocked on IFDs.
  , eiWorkerMs     :: !Int64
    -- ^ Worker-milliseconds alive.
  , eiAllWaitingMs :: !Int64
    -- ^ How long every live worker was waiting on an IFD at once
    -- (approximate).
  , eiWorkers      :: !Int
    -- ^ Worker processes spawned.
  , eiExitReasons  :: !(Map Text Int)
    -- ^ @max-memory@ (restarted to free memory), @exit@ (done), or @none@
    -- (no exit line: still running, or crashed).
  , eiMedianLifeMs :: !(Maybe Int64)
  , eiMedianJobs   :: !(Maybe Double)
  , eiMeanInitMs   :: !(Maybe Double)
  , eiJobs         :: !Int
  , eiJobErrors    :: !Int
  , eiMaxRssMb     :: !(Maybe Int)
  , eiBuilds       :: !Int
    -- ^ Builds the evaluation started (IFDs and their dependencies).
  , eiBuildFailures :: !Int
  , eiTop          :: !Table
    -- ^ The IFDs workers waited on longest.
  , eiFailed       :: !Table
    -- ^ IFDs that failed, and how many times each was tried.
  , eiBuildErrors  :: !Table
    -- ^ Failed builds, by derivation and builder.
  , eiNow          :: !(Maybe IfdNow)
    -- ^ While it runs.
  } deriving stock (Show, Eq, Generic)

instance ToJSON EvalIfd where toJSON = genericToJSON apiOptions
instance FromJSON EvalIfd where parseJSON = genericParseJSON apiOptions
