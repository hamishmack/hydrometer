{-# LANGUAGE DeriveGeneric #-}
-- | Server configuration, read from a JSON file (the NixOS module writes it).
module Hydrometer.Server.Config
  ( Config (..)
  , AgentConfig (..)
  , readConfig
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data AgentConfig = AgentConfig
  { name :: Text
    -- ^ The machine name as hydra knows it (buildsteps.machine).
  , url  :: Text
    -- ^ e.g. @http://aarch64-darwin-6.lan:9184@.
  } deriving stock (Show, Generic)

instance FromJSON AgentConfig

data Config = Config
  { db            :: Text
    -- ^ libpq connection string for the owner role.
  , readOnlyDb    :: Maybe Text
    -- ^ Connection string for the read-only role used by ad-hoc SQL (the
    -- MCP @sql@ tool and @hydrometer query@).  Defaults to 'db'.
  , listen        :: Maybe Text
    -- ^ @HOST:PORT@, default @127.0.0.1:9185@.
  , static        :: Maybe FilePath
    -- ^ Directory holding the built UI.
  , queueRunner   :: Maybe Text
    -- ^ Base URL of the queue runner's REST API, e.g. @http://[::1]:8080@.
  , agents        :: [AgentConfig]
  , retentionDays :: Maybe Int
    -- ^ Days of raw samples to keep (default 14).  Minute rollups and run
    -- totals are kept for a year.
  , pollSeconds   :: Maybe Int
  , cacheUrl      :: Maybe Text
    -- ^ The binary cache finished outputs should reach, e.g.
    -- @https://cache.zw3rk.com@; recent outputs are checked against it.
  , logUnits      :: Maybe [Text]
    -- ^ systemd units whose journal is scanned for errors and warnings.
  , maxConcurrentEvals :: Maybe Int
    -- ^ hydra-evaluator's max_concurrent_evals (default 1), for the
    -- pending evaluations' estimated start times.
  , substituters  :: Maybe [Text]
    -- ^ The public binary caches nix substitutes from, in the order it tries
    -- them, to say where a substituted path came from.
  } deriving stock (Show, Generic)

instance FromJSON Config

readConfig :: FilePath -> IO Config
readConfig f = eitherDecodeFileStrict f >>= either (fail . (("bad config " <> f <> ": ") <>)) pure
