{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | The agent wire protocol.  An agent samples its host every few seconds and
-- keeps the samples in a ring buffer; the server pulls them with
-- @GET /samples?since=<seq>@.  Times are Unix epoch milliseconds so the same
-- types serialise cheaply to the browser.
--
-- Counters that only make sense as rates (CPU time, bytes moved) are sent as
-- deltas over the sample interval ('hIntervalMs'); levels (memory, open files)
-- are sent as the value at the sample time.
module Hydrometer.Types
  ( EpochMs
  , AgentInfo (..)
  , BuilderInfo (..)
  , SamplesResponse (..)
  , Sample (..)
  , HostStats (..)
  , RunKind (..)
  , runKindText
  , RunStats (..)
  , ClassStats (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Options (..), defaultOptions, genericParseJSON, genericToJSON, withText)
import Data.Aeson qualified as A
import Data.Char (isUpper, toLower)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

type EpochMs = Int64

-- | Field names drop their one-letter-plus-capital prefix (@hCpuUser@ →
-- @cpuUser@) on the wire.
wireOptions :: Options
wireOptions = defaultOptions { fieldLabelModifier = dropPrefix, omitNothingFields = True }
  where
    dropPrefix s = case dropWhile (not . isUpper) s of
      (c:cs) -> toLower c : cs
      [] -> s

data AgentInfo = AgentInfo
  { aHostname     :: !Text
    -- ^ The name Hydra uses for this machine (e.g. @aarch64-darwin-9@).
  , aOs           :: !Text
  , aOsVersion    :: !Text
  , aCpuCount     :: !Int
  , aMemTotal     :: !Int64
  , aAgentVersion :: !Text
  , aBootTime     :: !EpochMs
  , aStartTime    :: !EpochMs
    -- ^ When this agent process started; a change tells the server that
    -- sequence numbers restarted.
  , aBuilders     :: ![BuilderInfo]
    -- ^ The hydra queue builders running on this host (one per set of
    -- systems), read from their command lines.
  } deriving stock (Show, Eq, Generic)

data BuilderInfo = BuilderInfo
  { bSystems :: ![Text]
  , bMaxJobs :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToJSON BuilderInfo where toJSON = genericToJSON wireOptions
instance FromJSON BuilderInfo where parseJSON = genericParseJSON wireOptions

instance ToJSON AgentInfo where toJSON = genericToJSON wireOptions
instance FromJSON AgentInfo where parseJSON = genericParseJSON wireOptions

data SamplesResponse = SamplesResponse
  { srAgent   :: !AgentInfo
  , srOldest  :: !Int64
    -- ^ The oldest sequence number still in the ring buffer.  If it is past
    -- the @since@ the server asked for, samples were lost.
  , srSamples :: ![Sample]
  } deriving stock (Show, Eq, Generic)

instance ToJSON SamplesResponse where toJSON = genericToJSON wireOptions
instance FromJSON SamplesResponse where parseJSON = genericParseJSON wireOptions

data Sample = Sample
  { sSeq     :: !Int64
  , sTime    :: !EpochMs
  , sHost    :: !HostStats
  , sRuns    :: ![RunStats]
  , sClasses :: ![ClassStats]
  } deriving stock (Show, Eq, Generic)

instance ToJSON Sample where toJSON = genericToJSON wireOptions
instance FromJSON Sample where parseJSON = genericParseJSON wireOptions

data HostStats = HostStats
  { hIntervalMs   :: !Int64
  , hCpuUserMs    :: !Int64
    -- ^ CPU time summed over all cores during the interval.
  , hCpuSystemMs  :: !Int64
  , hCpuIdleMs    :: !Int64
  , hLoad1        :: !Double
  , hLoad5        :: !Double
  , hLoad15       :: !Double
  , hMemTotal     :: !Int64
  , hMemUsed      :: !Int64
    -- ^ Memory not available for new allocations without reclaiming:
    -- darwin: active + wired + compressor; linux: total - MemAvailable.
  , hMemCompressed :: !Int64
  , hMemPressure  :: !(Maybe Double)
    -- ^ 0 (none) .. 100 (critical).  darwin: 100 - kern.memorystatus_level;
    -- linux: memory PSI "some" avg10.
  , hCpuPressure  :: !(Maybe Double)
  , hIoPressure   :: !(Maybe Double)
  , hSwapUsed     :: !Int64
  , hSwapTotal    :: !Int64
  , hPageouts     :: !Int64
    -- ^ Pages swapped/paged out during the interval.
  , hNetRxBytes   :: !Int64
  , hNetTxBytes   :: !Int64
  , hDiskReadBytes :: !Int64
  , hDiskWriteBytes :: !Int64
  , hOpenFiles    :: !(Maybe Int64)
  , hMaxFiles     :: !(Maybe Int64)
  , hFreeVnodes   :: !(Maybe Int64)
    -- ^ kern.free_vnodes (macOS).  None free: opening a file fails with ENFILE.
  , hMaxVnodes    :: !(Maybe Int64)
    -- ^ kern.maxvnodes (macOS).
  , hStoreFree    :: !(Maybe Int64)
  , hStoreTotal   :: !(Maybe Int64)
  } deriving stock (Show, Eq, Generic)

instance ToJSON HostStats where toJSON = genericToJSON wireOptions
instance FromJSON HostStats where parseJSON = genericParseJSON wireOptions

data RunKind
  = VmBuild      -- ^ A linux derivation built in a nix-linux-builder VM.
  | NativeBuild  -- ^ A derivation built directly by the local nix-daemon.
  | Evaluation   -- ^ A hydra jobset evaluation (hydra-eval-jobset and its workers).
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

runKindText :: RunKind -> Text
runKindText = \case
  VmBuild -> "vm"
  NativeBuild -> "native"
  Evaluation -> "eval"

instance ToJSON RunKind where toJSON = A.String . runKindText
instance FromJSON RunKind where
  parseJSON = withText "RunKind" $ \case
    "vm" -> pure VmBuild
    "native" -> pure NativeBuild
    "eval" -> pure Evaluation
    t -> fail ("unknown run kind " <> show t)

-- | One unit of attributable work alive during the interval.
data RunStats = RunStats
  { rKey       :: !Text
    -- ^ Unique on this host: @<kind>:<root pid>:<root start ms>@.
  , rKind      :: !RunKind
  , rName      :: !Text
    -- ^ Derivation name (or @project:jobset@ for an evaluation).
  , rOutputs   :: ![Text]
    -- ^ Output store paths; these join to hydra's @buildstepoutputs.path@.
  , rSystem    :: !(Maybe Text)
  , rStartTime :: !EpochMs
  , rCpuMs     :: !Int64
    -- ^ CPU time during the interval, including processes that exited.
  , rRss       :: !Int64
  , rFootprint :: !Int64
    -- ^ darwin phys_footprint (what memory pressure is driven by); linux: PSS
    -- is too costly to read, so this is RSS.
  , rDiskReadBytes  :: !Int64
  , rDiskWriteBytes :: !Int64
  , rNetRxBytes :: !Int64
  , rNetTxBytes :: !Int64
  , rProcs     :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToJSON RunStats where toJSON = genericToJSON wireOptions
instance FromJSON RunStats where parseJSON = genericParseJSON wireOptions

-- | Usage by processes that are not part of a run, grouped by role:
-- @nix-daemon@ (substitution, store I/O), @hydra-builder@ (input import,
-- output upload), @peernix@, @agent@, @vm-idle@, @other@.
data ClassStats = ClassStats
  { cClass     :: !Text
  , cCpuMs     :: !Int64
  , cRss       :: !Int64
  , cDiskReadBytes  :: !Int64
  , cDiskWriteBytes :: !Int64
  , cNetRxBytes :: !Int64
  , cNetTxBytes :: !Int64
  , cProcs     :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToJSON ClassStats where toJSON = genericToJSON wireOptions
instance FromJSON ClassStats where parseJSON = genericParseJSON wireOptions
