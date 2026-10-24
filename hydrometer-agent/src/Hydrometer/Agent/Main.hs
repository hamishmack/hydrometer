-- | The agent: sample every few seconds into a ring buffer and serve it.
--
-- The agent never connects out.  The server pulls @GET /samples?since=N@,
-- so a server restart or outage loses nothing that is still in the buffer.
module Hydrometer.Agent.Main
  ( main
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (status200, status400, status404, hContentType)
import Network.Wai (Application, ResponseReceived, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Network.HostName (getHostName)
import GHC.Conc (getNumProcessors)

import Hydrometer.Agent.Platform
import Hydrometer.Agent.Sampler
import Hydrometer.Agent.Sys (HostRaw (..), Proc (..), ProcArgs (..), parseBuilderToml)
import Hydrometer.Types

data Config = Config
  { cfgHost     :: String
  , cfgPort     :: Int
  , cfgName     :: Maybe Text
  , cfgInterval :: Int
  , cfgBuffer   :: Int
  , cfgStore    :: Text
  }

defaultConfig :: Config
defaultConfig = Config "*" 9184 Nothing 5 720 "/nix"

usage :: String
usage = unlines
  [ "usage: hydrometer-agent [--listen HOST:PORT] [--name NAME] [--interval SECONDS]"
  , "                        [--buffer SAMPLES] [--store PATH]"
  , ""
  , "  --listen    address to serve on (default *:9184)"
  , "  --name      machine name as hydra knows it (default: the host name)"
  , "  --interval  seconds between samples (default 5)"
  , "  --buffer    samples kept for the server to pull (default 720 = 1 hour)"
  , "  --store     filesystem whose free space is reported (default /nix)"
  ]

parseArgs :: Config -> [String] -> Either String Config
parseArgs c = \case
  [] -> Right c
  ("--listen" : v : r) -> let (h, p) = break (== ':') (reverse v)
                          in parseArgs c { cfgHost = reverse (drop 1 p), cfgPort = read (reverse h) } r
  ("--name" : v : r) -> parseArgs c { cfgName = Just (T.pack v) } r
  ("--interval" : v : r) -> parseArgs c { cfgInterval = read v } r
  ("--buffer" : v : r) -> parseArgs c { cfgBuffer = read v } r
  ("--store" : v : r) -> parseArgs c { cfgStore = T.pack v } r
  (a : _) -> Left ("unexpected argument " <> a)

nowMs :: IO EpochMs
nowMs = round . (* 1000) <$> getPOSIXTime

main :: IO ()
main = do
  args <- getArgs
  cfg <- case parseArgs defaultConfig args of
    Right c -> pure c
    Left e -> hPutStrLn stderr (e <> "\n" <> usage) >> exitFailure
  name <- maybe (T.pack . takeWhile (/= '.') <$> getHostName) pure (cfgName cfg)
  pl <- platform (cfgStore cfg) (cfgInterval cfg)
  boot <- readBootTimeMs
  ver <- osVersion
  started <- nowMs
  h0 <- plReadHost pl
  let info = AgentInfo
        { aHostname = name, aOs = osName, aOsVersion = ver
        , aCpuCount = 0, aMemTotal = hrMemTotal h0
        , aAgentVersion = "0.1.0.0", aBootTime = boot, aStartTime = started
        , aBuilders = [] }
  cpus <- getNumProcessors
  buf <- newTVarIO (Seq.empty :: Seq Sample)
  agentInfo <- newTVarIO info { aCpuCount = cpus }
  void $ forkIO $ sampler cfg pl buf
  void $ forkIO $ forever $ do
    bs <- try (findBuilders pl) :: IO (Either SomeException [BuilderInfo])
    either (const (pure ())) (\b -> atomically (modifyTVar' agentInfo (\i -> i { aBuilders = b }))) bs
    threadDelay 60000000
  hPutStrLn stderr ("hydrometer-agent " <> T.unpack name <> " listening on " <> cfgHost cfg <> ":" <> show (cfgPort cfg))
  let settings = Warp.setHost (fromString (cfgHost cfg)) $ Warp.setPort (cfgPort cfg) Warp.defaultSettings
  Warp.runSettings settings (app agentInfo buf)

sampler :: Config -> Platform -> TVar (Seq Sample) -> IO ()
sampler cfg pl buf = go 1 initialState
  where
    period = fromIntegral (cfgInterval cfg) * 1000 :: Int64
    go :: Int64 -> SamplerState -> IO ()
    go n st = do
      t <- nowMs
      r <- try (takeSample pl n t st) :: IO (Either SomeException (Maybe Sample, SamplerState))
      (n', st') <- case r of
        Left e -> hPutStrLn stderr ("sample failed: " <> show e) >> pure (n, st)
        Right (Nothing, s) -> pure (n, s)
        Right (Just s, s') -> do
          atomically $ modifyTVar' buf $ \q ->
            let q' = q |> s in if Seq.length q' > cfgBuffer cfg then Seq.drop 1 q' else q'
          pure (n + 1, s')
      -- Align to the interval so samples from different hosts line up.
      t' <- nowMs
      let next = (t' `div` period + 1) * period
      threadDelay (fromIntegral (next - t') * 1000)
      go n' st'

app :: TVar AgentInfo -> TVar (Seq Sample) -> Application
app infoVar buf req respond = readTVarIO infoVar >>= \info -> case pathInfo req of
  ["samples"] -> do
    let since = case lookup "since" (queryString req) of
          Just (Just v) | Just (n, _) <- BC.readInteger v -> Just (fromIntegral n :: Int64)
          _ -> Nothing
    case since of
      Nothing -> respond (responseLBS status400 [] "since=<seq> required\n")
      Just s -> do
        q <- readTVarIO buf
        let newer = Seq.dropWhileL ((<= s) . sSeq) q
            oldest = maybe 0 sSeq (Seq.lookup 0 q)
        json (SamplesResponse info oldest (toList (Seq.take 240 newer)))
  ["now"] -> do
    q <- readTVarIO buf
    case Seq.lookup (Seq.length q - 1) q of
      Just s -> json (SamplesResponse info (sSeq s) [s])
      Nothing -> json (SamplesResponse info 0 [])
  ["health"] -> respond (responseLBS status200 [] "ok\n")
  [] -> json info
  _ -> respond (responseLBS status404 [] "not found\n")
  where
    json :: A.ToJSON a => a -> IO ResponseReceived
    json v = respond (responseLBS status200 [(hContentType, "application/json")] (A.encode v))

-- | The hydra queue builders on this host and their capacity: from the
-- config file named by @--config-path@ (current builders), else from the
-- command line (@--systems S@ repeated, @--max-jobs N@; older ones).
findBuilders :: Platform -> IO [BuilderInfo]
findBuilders pl = do
  ps <- plReadProcs pl
  let builders = [ p | p <- ps, pComm p == "hydra-builder" || "/bin/hydra-builder" `T.isSuffixOf` pPath p ]
  infos <- mapM (plReadProcArgs pl . pPid) builders
  mapM builderInfo [ argv | Just (ProcArgs argv _) <- infos ]
  where
    builderInfo argv = case configPathOf argv of
      Just path -> do
        r <- try (T.readFile (T.unpack path)) :: IO (Either SomeException Text)
        pure $ case parseBuilderToml <$> r of
          Right (mj, sys) -> BuilderInfo (if null sys then systemsOf argv else sys) (maybe (maxJobsOf argv) id mj)
          Left _ -> BuilderInfo (systemsOf argv) (maxJobsOf argv)
      Nothing -> pure (BuilderInfo (systemsOf argv) (maxJobsOf argv))
    configPathOf = \case
      ("--config-path" : path : _) -> Just path
      (a : r) | Just path <- T.stripPrefix "--config-path=" a -> Just path
              | otherwise -> configPathOf r
      [] -> Nothing
    systemsOf = \case
      ("--systems" : s : r) -> s : systemsOf r
      (_ : r) -> systemsOf r
      [] -> []
    maxJobsOf = \case
      ("--max-jobs" : n : _) | [(v, "")] <- reads (T.unpack n) -> v
      (_ : r) -> maxJobsOf r
      [] -> 1
