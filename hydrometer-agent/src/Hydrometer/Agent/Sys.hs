-- | Platform-neutral raw readings.  The platform modules
-- ("Hydrometer.Agent.Darwin", "Hydrometer.Agent.Linux") produce these; the
-- sampler turns pairs of readings into the deltas on the wire.
module Hydrometer.Agent.Sys
  ( Proc (..)
  , HostRaw (..)
  , ProcArgs (..)
  , parseProcArgs2
  , parseNulSeparated
  , parseBuilderToml
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T

-- | A process as seen at one instant.  CPU and disk figures are cumulative
-- over the process lifetime.
data Proc = Proc
  { pPid        :: !Int
  , pPpid       :: !Int
  , pUid        :: !Int
  , pStartMs    :: !Int64
  , pCpuNs      :: !Int64
  , pChildCpuNs :: !Int64
    -- ^ CPU of children this process has reaped.
  , pRss        :: !Int64
  , pFootprint  :: !Int64
  , pDiskRead   :: !Int64
  , pDiskWrite  :: !Int64
  , pComm       :: !Text
  , pPath       :: !Text
  } deriving stock (Show, Eq)

-- | Host-wide readings.  Counters are cumulative since boot; the sampler
-- differences them.
data HostRaw = HostRaw
  { hrCpuUserMs   :: !Int64
  , hrCpuSystemMs :: !Int64
  , hrCpuIdleMs   :: !Int64
  , hrLoad        :: !(Double, Double, Double)
  , hrMemTotal    :: !Int64
  , hrMemUsed     :: !Int64
  , hrMemCompressed :: !Int64
  , hrMemPressure :: !(Maybe Double)
  , hrCpuPressure :: !(Maybe Double)
  , hrIoPressure  :: !(Maybe Double)
  , hrSwapUsed    :: !Int64
  , hrSwapTotal   :: !Int64
  , hrPageouts    :: !Int64
  , hrNetRx       :: !Int64
  , hrNetTx       :: !Int64
  , hrDiskRead    :: !Int64
  , hrDiskWrite   :: !Int64
  , hrOpenFiles   :: !(Maybe Int64)
  , hrMaxFiles    :: !(Maybe Int64)
  , hrFreeVnodes  :: !(Maybe Int64)
  , hrMaxVnodes   :: !(Maybe Int64)
  , hrStoreFree   :: !(Maybe Int64)
  , hrStoreTotal  :: !(Maybe Int64)
  } deriving stock (Show, Eq)

data ProcArgs = ProcArgs
  { paArgv :: ![Text]
  , paEnv  :: ![(Text, Text)]
  } deriving stock (Show, Eq)

decode :: ByteString -> Text
decode = T.decodeUtf8With T.lenientDecode

-- | Parse a darwin @KERN_PROCARGS2@ blob: a native-endian 32-bit argc, the
-- exec path, NUL padding, argc arguments, then environment strings up to an
-- empty string.
parseProcArgs2 :: ByteString -> Maybe ProcArgs
parseProcArgs2 bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let argc = fromIntegral (BS.index bs 0) + 256 * fromIntegral (BS.index bs 1)
               + 65536 * fromIntegral (BS.index bs 2) + 16777216 * fromIntegral (BS.index bs 3) :: Int
          rest = BS.drop 4 bs
          afterPath = BS.dropWhile (== 0) (BS.dropWhile (/= 0) rest)
          strs = BS.split 0 afterPath
          (argv, envs) = splitAt argc strs
          env = [ splitEnv e | e <- takeWhile (not . BS.null) envs, BC.elem '=' e ]
      in Just ProcArgs { paArgv = map decode argv, paEnv = env }
  where
    splitEnv e = let (k, v) = BC.break (== '=') e in (decode k, decode (BS.drop 1 v))

-- | Parse NUL-separated strings as in linux @/proc/<pid>/cmdline@ and
-- @/proc/<pid>/environ@.
parseNulSeparated :: ByteString -> [Text]
parseNulSeparated = map decode . filter (not . BS.null) . BS.split 0

-- | @maxJobs@ and @systems@ from a hydra-builder config file
-- (@--config-path@), which holds them since the builder stopped taking
-- @--max-jobs@ and @--systems@ flags.  It is TOML written by nix
-- (@pkgs.formats.toml@): one @key = value@ per line, string arrays on one
-- line, so a full TOML parser is not needed.
parseBuilderToml :: Text -> (Maybe Int, [Text])
parseBuilderToml t = (maxJobs, systems)
  where
    kvs = [ (T.strip k, T.strip (T.drop 1 v))
          | l <- T.lines t, let (k, v) = T.breakOn "=" l, not (T.null v) ]
    maxJobs = case lookup "maxJobs" kvs of
      Just v | [(n, "")] <- reads (T.unpack v) -> Just n
      _ -> Nothing
    systems = case lookup "systems" kvs of
      Just v | Just inner <- T.stripPrefix "[" v >>= T.stripSuffix "]" ->
        [ s | x <- T.splitOn "," inner
            , let s = T.dropAround (== '"') (T.strip x), not (T.null s) ]
      _ -> []
