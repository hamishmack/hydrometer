-- | Who holds the Nix garbage collector's lock on this host.
--
-- While a garbage collection deletes paths, nothing can be added to the
-- store: every addition waits for the lock (with the store below min-free,
-- each one queues an auto-GC behind it).  On the queue runner's host that
-- stalls the builders' uploads, and builds whose upload times out are redone.
-- Read from @/proc/locks@ and @/proc/PID@, which any user can read (Linux
-- only; elsewhere this finds nothing).
module Hydrometer.Server.GcLock
  ( GcHolder (..)
  , gcLockHolders
  ) where

import Control.Exception (SomeException, try)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Numeric (showHex)
import System.Posix.Files (deviceID, fileID, getFileStatus)
import Text.Read (readMaybe)

data GcHolder = GcHolder
  { ghPid :: !Int
  , ghCommand :: !Text
    -- ^ The holder's command line.  For a nix-daemon worker, the command of
    -- the client it serves.
  , ghReadOnly :: !Bool
    -- ^ @nix-store --gc --print-*@: holds the lock but deletes nothing, and
    -- additions do not wait for it.
  , ghSinceMs :: !(Maybe Int64)
    -- ^ When the holding process started (epoch ms): an upper bound on how
    -- long the lock has been held.
  }

gcLockPath :: FilePath
gcLockPath = "/nix/var/nix/gc.lock"

gcLockHolders :: IO [GcHolder]
gcLockHolders = do
  r <- try $ do
    st <- getFileStatus gcLockPath
    let dev = fromIntegral (deviceID st) :: Integer
        -- The kernel's MAJOR()/MINOR() of a userspace dev_t.
        major = (dev `shiftR` 8) .&. 0xfff
        minor = (dev .&. 0xff) .|. ((dev `shiftR` 12) .&. 0xfff00)
        key = T.pack (hex2 major <> ":" <> hex2 minor <> ":" <> show (fromIntegral (fileID st) :: Integer))
    locks <- T.readFile "/proc/locks"
    btime <- bootTime
    let pids = [ p | l <- T.lines locks, Just p <- [holder key l] ]
    mapM (describe btime) pids
  pure (either (const [] :: SomeException -> [GcHolder]) id r)
  where
    hex2 n = let s = showHex n "" in replicate (2 - length s) '0' <> s
    -- "1: FLOCK  ADVISORY  WRITE 1362118 00:1c:264 0 EOF"; waiters have "->".
    holder key l = case T.words l of
      (_ : rest) | "->" `notElem` take 1 rest, (_ : _ : _ : pid : dev : _) <- rest, dev == key -> readMaybe (T.unpack pid)
      _ -> Nothing

describe :: Maybe Int64 -> Int -> IO GcHolder
describe btime pid = do
  argv <- cmdline pid
  command <- case argv of
    -- A daemon worker is named after the client it serves.
    (d : client : _) | "nix-daemon" `T.isSuffixOf` d, Just c <- readMaybe (T.unpack client) -> do
      ca <- cmdline c
      pure (if null ca then T.unwords argv else "nix-daemon for " <> T.unwords ca)
    _ -> pure (T.unwords argv)
  since <- startedMs btime pid
  pure GcHolder
    { ghPid = pid
    , ghCommand = T.take 200 command
    , ghReadOnly = "--print-" `T.isInfixOf` command
    , ghSinceMs = since
    }

cmdline :: Int -> IO [Text]
cmdline pid = do
  r <- try (T.readFile ("/proc/" <> show pid <> "/cmdline")) :: IO (Either SomeException Text)
  pure (either (const []) (filter (not . T.null) . T.splitOn "\0") r)

-- | Boot time (epoch seconds), from @/proc/stat@.
bootTime :: IO (Maybe Int64)
bootTime = do
  r <- try (T.readFile "/proc/stat") :: IO (Either SomeException Text)
  pure $ case r of
    Right s -> case [ w | l <- T.lines s, ["btime", w] <- [T.words l] ] of
      (w : _) -> readMaybe (T.unpack w)
      [] -> Nothing
    Left _ -> Nothing

-- | A process's start time (epoch ms): field 22 of @/proc/PID/stat@, in clock
-- ticks (100 a second on Linux) since boot.
startedMs :: Maybe Int64 -> Int -> IO (Maybe Int64)
startedMs btime pid = do
  r <- try (T.readFile ("/proc/" <> show pid <> "/stat")) :: IO (Either SomeException Text)
  pure $ do
    s <- either (const Nothing) Just r
    b <- btime
    -- The command name (field 2) may contain spaces; fields resume after ')'.
    let fields = T.words (snd (T.breakOnEnd ")" s))
    ticks <- case drop 19 fields of
      (t : _) -> readMaybe (T.unpack t) :: Maybe Int64
      [] -> Nothing
    pure ((b * 100 + ticks) * 10)
