-- | Linux readings from @/proc@.
module Hydrometer.Agent.Linux
  ( readHost
  , readProcs
  , readProcArgs
  , readBootTimeMs
  , osVersion
  ) where

import Control.Exception (SomeException, try, evaluate)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Foreign
import Foreign.C
import System.Directory (listDirectory)
import System.Posix.Files (fileOwner, getFileStatus, readSymbolicLink)

import Hydrometer.Agent.Sys

foreign import ccall unsafe "hm_fs_space" c_fs_space :: CString -> Ptr Int64 -> IO CInt

clockTick, pageSize :: Int64
clockTick = 100
pageSize = 4096

tryIO :: forall a. IO a -> IO (Maybe a)
tryIO act = either (const Nothing) Just <$> (try (act >>= evaluate) :: IO (Either SomeException a))

readFileStrict :: FilePath -> IO (Maybe BS.ByteString)
readFileStrict = tryIO . BS.readFile

readInt :: BS.ByteString -> Int64
readInt = maybe 0 (fromIntegral . fst) . BC.readInteger

-- | @key: value kB@ style files (meminfo).
keyed :: BS.ByteString -> [(BS.ByteString, Int64)]
keyed bs = [ (BC.takeWhile (/= ':') l, readInt (head' (drop 1 (BC.words l)))) | l <- BC.lines bs ]
  where head' = \case (x:_) -> x; [] -> "0"

readPressure :: FilePath -> IO (Maybe Double)
readPressure f = do
  mb <- readFileStrict f
  pure $ do
    bs <- mb
    l <- case BC.lines bs of (x:_) -> Just x; [] -> Nothing
    w <- case filter ("avg10=" `BS.isPrefixOf`) (BC.words l) of (x:_) -> Just x; [] -> Nothing
    pure (read (BC.unpack (BS.drop 6 w)))

physicalInterface :: BS.ByteString -> Bool
physicalInterface n = not (any (`BS.isPrefixOf` n)
  ["lo", "veth", "docker", "br-", "virbr", "tap", "tun", "wg", "vnet"])

wholeDisk :: String -> Bool
wholeDisk n
  | "nvme" `isPrefixOf` n = 'p' `notElem` drop 5 n  -- nvme0n1, not nvme0n1p2
  | any (`isPrefixOf` n) ["sd", "vd", "xvd"] = not (isDigit (last n))
  | otherwise = False

readHost :: Text -> IO HostRaw
readHost storePath = do
  stat <- fromMaybe "" <$> readFileStrict "/proc/stat"
  let cpuFields = case BC.words <$> filter ("cpu " `BS.isPrefixOf`) (BC.lines stat) of
        ((_:fs):_) -> map readInt fs
        _ -> []
      field k = if length cpuFields > k then cpuFields !! k else 0
      ms x = x * 1000 `div` clockTick
  loadavg <- fromMaybe "" <$> readFileStrict "/proc/loadavg"
  let load = case BC.words loadavg of
        (a:b:c:_) -> (read (BC.unpack a), read (BC.unpack b), read (BC.unpack c))
        _ -> (0, 0, 0)
  mem <- keyed . fromMaybe "" <$> readFileStrict "/proc/meminfo"
  let kb k = maybe 0 (* 1024) (lookup k mem)
  vmstat <- fromMaybe "" <$> readFileStrict "/proc/vmstat"
  let vm k = sum [ readInt v | [n, v] <- map BC.words (BC.lines vmstat), n == k ]
  netdev <- fromMaybe "" <$> readFileStrict "/proc/net/dev"
  let ifs = [ (BC.strip n, map readInt (BC.words rest))
            | l <- drop 2 (BC.lines netdev), let (n, r) = BC.break (== ':') l, let rest = BS.drop 1 r ]
      phys = [ fs | (n, fs) <- ifs, physicalInterface n, length fs >= 9 ]
  disks <- fromMaybe "" <$> readFileStrict "/proc/diskstats"
  let dsk = [ (readInt (ws !! 5) * 512, readInt (ws !! 9) * 512)
            | l <- BC.lines disks, let ws = BC.words l, length ws >= 10, wholeDisk (BC.unpack (ws !! 2)) ]
  fileNr <- maybe [] (map readInt . BC.words) <$> readFileStrict "/proc/sys/fs/file-nr"
  space <- withCString (T.unpack storePath) $ \cs -> allocaArray 2 $ \p -> do
    r <- c_fs_space cs p
    if r == 0 then (\xs -> case xs of [a, b] -> Just (a, b); _ -> Nothing) <$> peekArray 2 p else pure Nothing
  memP <- readPressure "/proc/pressure/memory"
  cpuP <- readPressure "/proc/pressure/cpu"
  ioP <- readPressure "/proc/pressure/io"
  pure HostRaw
    { hrCpuUserMs = ms (field 0 + field 1)
    , hrCpuSystemMs = ms (field 2 + field 5 + field 6 + field 7)
    , hrCpuIdleMs = ms (field 3 + field 4)
    , hrLoad = load
    , hrMemTotal = kb "MemTotal"
    , hrMemUsed = kb "MemTotal" - kb "MemAvailable"
    , hrMemCompressed = 0
    , hrMemPressure = memP
    , hrCpuPressure = cpuP
    , hrIoPressure = ioP
    , hrSwapUsed = kb "SwapTotal" - kb "SwapFree"
    , hrSwapTotal = kb "SwapTotal"
    , hrPageouts = vm "pswpout"
    , hrNetRx = sum [ fs !! 0 | fs <- phys ]
    , hrNetTx = sum [ fs !! 8 | fs <- phys ]
    , hrDiskRead = sum (map fst dsk)
    , hrDiskWrite = sum (map snd dsk)
    , hrOpenFiles = case fileNr of (a:_) -> Just a; _ -> Nothing
    , hrMaxFiles = case fileNr of [_, _, m] -> Just m; _ -> Nothing
    , hrFreeVnodes = Nothing
    , hrMaxVnodes = Nothing
    , hrStoreFree = fst <$> space
    , hrStoreTotal = snd <$> space
    }

readBootTimeMs :: IO Int64
readBootTimeMs = do
  stat <- fromMaybe "" <$> readFileStrict "/proc/stat"
  pure $ case [ readInt v | [k, v] <- map BC.words (BC.lines stat), k == "btime" ] of
    (b:_) -> b * 1000
    [] -> 0

readProcs :: IO [Proc]
readProcs = do
  boot <- readBootTimeMs
  pids <- mapMaybe readPid <$> listDirectory "/proc"
  catMaybes <$> forM pids (readProc boot)
  where
    readPid s = if not (null s) && all isDigit s then Just (read s :: Int) else Nothing

readProc :: Int64 -> Int -> IO (Maybe Proc)
readProc boot pid = do
  let dir = "/proc/" <> show pid
  mstat <- readFileStrict (dir <> "/stat")
  case mstat of
    Nothing -> pure Nothing
    Just stat -> do
      -- comm may contain spaces and parentheses; split at the LAST ')'.
      let (pre, post) = BS.breakEnd (== 41) stat
          comm = BS.drop 1 (BC.dropWhile (/= '(') (BS.take (BS.length pre - 1) pre))
          fs = BC.words post   -- fs !! 0 is the state (field 3)
          f k = if length fs > k - 3 then readInt (fs !! (k - 3)) else 0
      owner <- tryIO (fileOwner <$> getFileStatus dir)
      io <- maybe [] (map (\l -> (BC.takeWhile (/= ':') l, readInt (BC.dropWhile (== ' ') (BS.drop 1 (BC.dropWhile (/= ':') l))))) . BC.lines)
              <$> readFileStrict (dir <> "/io")
      exe <- tryIO (readSymbolicLink (dir <> "/exe"))
      let tickNs x = x * (1000000000 `div` clockTick)
      pure $ Just Proc
        { pPid = pid
        , pPpid = fromIntegral (f 4)
        , pUid = maybe 0 fromIntegral owner
        , pStartMs = boot + f 22 * 1000 `div` clockTick
        , pCpuNs = tickNs (f 14 + f 15)
        , pChildCpuNs = tickNs (f 16 + f 17)
        , pRss = f 24 * pageSize
        , pFootprint = f 24 * pageSize
        , pDiskRead = fromMaybe 0 (lookup "read_bytes" io)
        , pDiskWrite = fromMaybe 0 (lookup "write_bytes" io)
        , pComm = T.decodeUtf8With T.lenientDecode comm
        , pPath = maybe "" T.pack exe
        }

readProcArgs :: Int -> IO (Maybe ProcArgs)
readProcArgs pid = do
  let dir = "/proc/" <> show pid
  cmd <- readFileStrict (dir <> "/cmdline")
  env <- readFileStrict (dir <> "/environ")
  pure $ case cmd of
    Nothing -> Nothing
    Just c -> Just ProcArgs
      { paArgv = parseNulSeparated c
      , paEnv = [ (k, T.drop 1 v) | e <- maybe [] parseNulSeparated env, let (k, v) = T.breakOn "=" e ]
      }

osVersion :: IO Text
osVersion = do
  r <- readFileStrict "/etc/os-release"
  pure $ case r of
    Nothing -> ""
    Just bs -> head' [ T.dropAround (== '"') (T.drop 1 v)
                     | l <- T.lines (T.decodeUtf8With T.lenientDecode bs)
                     , let (k, v) = T.breakOn "=" l, k == "PRETTY_NAME" ]
  where head' = \case (x:_) -> x; [] -> ""
