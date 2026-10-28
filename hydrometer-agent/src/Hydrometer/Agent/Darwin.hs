-- | macOS readings via @cbits/darwin.c@.
module Hydrometer.Agent.Darwin
  ( readHost
  , readProcs
  , readProcArgs
  , readBootTimeMs
  , osVersion
  ) where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Word (Word32, Word64)
import Foreign
import Foreign.C
import System.Process (readProcess)
import Control.Exception (SomeException, try)

import Hydrometer.Agent.Sys

foreign import ccall unsafe "hm_host_cpu" c_host_cpu :: Ptr Word64 -> IO CInt
foreign import ccall unsafe "hm_host_mem" c_host_mem :: Ptr Int64 -> IO CInt
foreign import ccall unsafe "hm_host_misc" c_host_misc :: Ptr Int64 -> IO CInt
foreign import ccall unsafe "hm_net_ifaces" c_net_ifaces :: Ptr CChar -> Ptr Word64 -> Ptr Word64 -> CInt -> IO CInt
foreign import ccall unsafe "hm_disk_io" c_disk_io :: Ptr Word64 -> IO CInt
foreign import ccall unsafe "hm_fs_space" c_fs_space :: CString -> Ptr Int64 -> IO CInt
foreign import ccall unsafe "hm_proc_size" c_proc_size :: IO CInt
foreign import ccall unsafe "hm_procs" c_procs :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "hm_procargs" c_procargs :: CInt -> Ptr CChar -> CInt -> IO CInt

-- | Interfaces whose traffic is internal to the host (loopback, the VM
-- bridges, tunnels, AirDrop) and would otherwise be double counted.
physicalInterface :: Text -> Bool
physicalInterface n = not (any (`T.isPrefixOf` n)
  ["lo", "bridge", "vmenet", "utun", "awdl", "llw", "anpi", "ap", "gif", "stf", "feth"])

readHost :: Text -> IO HostRaw
readHost storePath = do
  (u, s, i, nice) <- allocaArray 4 $ \p -> do
    _ <- c_host_cpu p
    [a, b, c, d] <- peekArray 4 p
    pure (a, b, c, d)
  mem <- allocaArray 9 $ \p -> do
    _ <- c_host_mem p
    peekArray 9 p
  misc <- allocaArray 5 $ \p -> do
    _ <- c_host_misc p
    peekArray 5 p
  (rx, tx) <- readNet
  (dr, dw) <- allocaArray 2 $ \p -> do
    _ <- c_disk_io p
    [a, b] <- peekArray 2 p
    pure (a, b)
  space <- withCString (T.unpack storePath) $ \cs -> allocaArray 2 $ \p -> do
    r <- c_fs_space cs p
    if r == 0 then (\[a, b] -> Just (a, b)) <$> peekArray 2 p else pure Nothing
  load <- allocaArray 3 $ \p -> do
    n <- c_getloadavg p 3
    if n == 3 then (\[a, b, c] -> (realToFrac a, realToFrac b, realToFrac c)) <$> peekArray 3 p
              else pure (0, 0, 0)
  memTotal <- sysctlInt64 "hw.memsize"
  let [used, compressed, _free, pageouts, swapouts, _page, swapUsed, swapTotal, level] = mem
      [numFiles, maxFiles, _boot, freeVnodes, maxVnodes] = misc
  pure HostRaw
    { hrCpuUserMs = fromIntegral (u + nice)
    , hrCpuSystemMs = fromIntegral s
    , hrCpuIdleMs = fromIntegral i
    , hrLoad = load
    , hrMemTotal = fromMaybe 0 memTotal
    , hrMemUsed = used
    , hrMemCompressed = compressed
    , hrMemPressure = if level < 0 then Nothing else Just (fromIntegral (100 - level))
    , hrCpuPressure = Nothing
    , hrIoPressure = Nothing
    , hrSwapUsed = swapUsed
    , hrSwapTotal = swapTotal
    , hrPageouts = pageouts + swapouts
    , hrNetRx = rx
    , hrNetTx = tx
    , hrDiskRead = fromIntegral dr
    , hrDiskWrite = fromIntegral dw
    , hrOpenFiles = if numFiles < 0 then Nothing else Just numFiles
    , hrMaxFiles = if maxFiles < 0 then Nothing else Just maxFiles
    , hrFreeVnodes = if freeVnodes < 0 then Nothing else Just freeVnodes
    , hrMaxVnodes = if maxVnodes < 0 then Nothing else Just maxVnodes
    , hrStoreFree = fst <$> space
    , hrStoreTotal = snd <$> space
    }

foreign import ccall unsafe "getloadavg" c_getloadavg :: Ptr CDouble -> CInt -> IO CInt
foreign import ccall unsafe "sysctlbyname" c_sysctlbyname :: CString -> Ptr () -> Ptr CSize -> Ptr () -> CSize -> IO CInt

sysctlInt64 :: String -> IO (Maybe Int64)
sysctlInt64 name = withCString name $ \cs -> alloca $ \p -> alloca $ \lenp -> do
  poke lenp (fromIntegral (sizeOf (0 :: Int64)))
  poke p 0
  r <- c_sysctlbyname cs (castPtr p) lenp nullPtr 0
  if r == 0 then Just <$> peek p else pure Nothing

readNet :: IO (Int64, Int64)
readNet = do
  let maxIf = 128
  allocaBytes (16 * maxIf) $ \names -> allocaArray maxIf $ \rxp -> allocaArray maxIf $ \txp -> do
    n <- fromIntegral <$> c_net_ifaces names rxp txp (fromIntegral maxIf)
    if n <= 0 then pure (0, 0) else do
      ifs <- forM [0 .. n - 1] $ \k -> do
        nm <- T.pack <$> peekCString (names `plusPtr` (16 * k))
        r <- peekElemOff rxp k
        t <- peekElemOff txp k
        pure (nm, r, t)
      let phys = [ (r, t) | (nm, r, t) <- ifs, physicalInterface nm ]
      pure (fromIntegral (sum (map fst phys)), fromIntegral (sum (map snd phys)))

readBootTimeMs :: IO Int64
readBootTimeMs = allocaArray 5 $ \p -> do
  _ <- c_host_misc p
  peekElemOff p 2

-- Offsets into struct hm_proc; see cbits/darwin.c.
readProcs :: IO [Proc]
readProcs = do
  sz <- fromIntegral <$> c_proc_size
  let maxProcs = 8192
  allocaBytes (sz * maxProcs) $ \buf -> do
    n <- fromIntegral <$> c_procs buf (fromIntegral maxProcs)
    forM [0 .. n - 1] $ \k -> do
      let p = buf `plusPtr` (k * sz)
      pid <- peekByteOff p 0 :: IO Int32
      ppid <- peekByteOff p 4 :: IO Int32
      uid <- peekByteOff p 8 :: IO Word32
      start <- peekByteOff p 16 :: IO Int64
      cpu <- peekByteOff p 24 :: IO Int64
      ccpu <- peekByteOff p 32 :: IO Int64
      rss <- peekByteOff p 40 :: IO Int64
      fp <- peekByteOff p 48 :: IO Int64
      dr <- peekByteOff p 56 :: IO Int64
      dw <- peekByteOff p 64 :: IO Int64
      comm <- peekCStringText (p `plusPtr` 72)
      path <- peekCStringText (p `plusPtr` 104)
      pure Proc
        { pPid = fromIntegral pid, pPpid = fromIntegral ppid, pUid = fromIntegral uid
        , pStartMs = start, pCpuNs = cpu, pChildCpuNs = ccpu
        , pRss = rss, pFootprint = fp, pDiskRead = dr, pDiskWrite = dw
        , pComm = comm, pPath = path }
  where
    peekCStringText p = T.decodeUtf8With T.lenientDecode <$> BS.packCString p

readProcArgs :: Int -> IO (Maybe ProcArgs)
readProcArgs pid = do
  let len = 262144
  allocaBytes len $ \buf -> do
    n <- c_procargs (fromIntegral pid) buf (fromIntegral len)
    if n <= 0 then pure Nothing
      else parseProcArgs2 <$> BS.packCStringLen (buf, fromIntegral n)

osVersion :: IO Text
osVersion = do
  r <- try (readProcess "/usr/bin/sw_vers" ["-productVersion"] "") :: IO (Either SomeException String)
  pure $ either (const "") (T.pack . filter (not . isSpace)) r
