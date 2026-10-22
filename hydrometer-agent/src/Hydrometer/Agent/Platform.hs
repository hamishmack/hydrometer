{-# LANGUAGE CPP #-}
-- | The platform backend for the host we are compiled for.
module Hydrometer.Agent.Platform
  ( platform
  , osName
  , osVersion
  , readBootTimeMs
  ) where


import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import System.Posix.Process (getProcessID)
import System.Posix.User (getAllUserEntries, userID, userName)

import Hydrometer.Agent.Sampler (Platform (..))
#if defined(darwin_HOST_OS)
import Hydrometer.Agent.Darwin
import Hydrometer.Agent.NetTop (netTopBytes)
#else
import Hydrometer.Agent.Linux
#endif

osName :: Text
#if defined(darwin_HOST_OS)
osName = "darwin"
#else
osName = "linux"
#endif

-- | Build users are named (_)nixbldN.  Linux nix with auto-allocate-uids
-- uses uids from 872415232 upwards, which have no user entries.
buildUids :: IO (Int -> Bool)
buildUids = do
  users <- getAllUserEntries
  let uids = IM.fromList [ (fromIntegral (userID u), ())
                         | u <- users
                         , T.isPrefixOf "nixbld" (T.dropWhile (== '_') (T.pack (userName u))) ]
  pure $ \uid -> IM.member uid uids || uid >= 872415232

platform :: Text -> Int -> IO Platform
platform storePath intervalSec = do
  isBuild <- buildUids
  self <- fromIntegral <$> getProcessID
#if defined(darwin_HOST_OS)
  let net = netTopBytes
      _ = intervalSec
#else
  let net = pure IM.empty
      _ = intervalSec
#endif
  pure Platform
    { plReadHost = readHost storePath
    , plReadProcs = readProcs
    , plReadProcArgs = readProcArgs
    , plNetBytes = net
    , plIsBuildUid = isBuild
    , plSelfPid = self
    }

