-- | Per-process network bytes on macOS.
--
-- There is no public API for per-process network accounting; @nettop@ reads
-- the private NetworkStatistics framework and can print CSV.  It
-- block-buffers when writing to a pipe, so instead of streaming we run a
-- one-shot @nettop -L 1@ (about 20 ms) at each sample.  The figures are
-- cumulative per process.
module Hydrometer.Agent.NetTop
  ( netTopBytes
  , parseNetTop
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (mapMaybe)
import System.Process (readProcess)

netTopBytes :: IO (IntMap (Int64, Int64))
netTopBytes = do
  r <- try (readProcess "/usr/bin/nettop" ["-P", "-L", "1", "-x", "-n", "-J", "bytes_in,bytes_out"] "")
         :: IO (Either SomeException String)
  pure $ either (const IM.empty) (parseNetTop . BC.pack) r

-- | Lines look like @name.pid,bytes_in,bytes_out,@; the process name may
-- itself contain dots and commas are not escaped, so split from the right.
parseNetTop :: BC.ByteString -> IntMap (Int64, Int64)
parseNetTop = IM.fromList . mapMaybe parseLine . drop 1 . BC.lines
  where
    parseLine l = case reverse (BC.split ',' l) of
      ("" : tx : rx : nameParts) -> do
        let name = BC.intercalate "," (reverse nameParts)
        (pid, rest) <- BC.readInt (BC.takeWhileEnd (/= '.') name)
        if BC.null rest then Just () else Nothing
        (r, _) <- BC.readInteger rx
        (t, _) <- BC.readInteger tx
        pure (pid, (fromIntegral r, fromIntegral t))
      _ -> Nothing
