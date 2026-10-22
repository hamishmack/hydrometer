-- | @hydrometer@: the collector/API server and its tools.
module Hydrometer.Server.Main
  ( main
  ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, managerConnCount, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Hydrometer.Server.Config
import Hydrometer.Server.Db
import Hydrometer.Server.Http
import Hydrometer.Server.Ingest
import Hydrometer.Server.Maintain
import Hydrometer.Server.Mcp
import Hydrometer.Server.QueueRunner

usage :: String
usage = unlines
  [ "usage: hydrometer COMMAND --config FILE [ARGS]"
  , ""
  , "  serve                 collect from agents and the queue runner, serve the API and UI"
  , "  migrate               create/upgrade the hydrometer schema and exit"
  , "  mcp                   Model Context Protocol server on stdio (for AI agents)"
  , "  query SQL             run read-only SQL and print {columns, rows} as JSON"
  , "  ingest MACHINE FILE   load a saved agent /samples response (for testing)"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd : "--config" : file : rest) -> do
      cfg <- readConfig file
      run cmd cfg rest
    _ -> hPutStrLn stderr usage >> exitFailure

listenAddr :: Config -> (String, Int)
listenAddr cfg =
  let l = fromMaybe "127.0.0.1:9185" (listen cfg)
      (h, p) = T.breakOnEnd ":" l
  in (T.unpack (T.dropEnd 1 h), read (T.unpack p))

run :: String -> Config -> [String] -> IO ()
run cmd cfg rest = case (cmd, rest) of
  ("migrate", []) -> do
    d <- openDb (db cfg) 1
    migrate d
    putStrLn "schema up to date"
  ("serve", []) -> do
    d <- openDb (db cfg) 8
    migrate d
    mgr <- newManager defaultManagerSettings { managerConnCount = 4 }
    agentStates <- newTVarIO M.empty
    queue <- newTVarIO emptyQueueState
    let (host, port) = listenAddr cfg
        pollSec = fromMaybe 5 (pollSeconds cfg)
        env = Env d agentStates queue (static cfg)
        settings = Warp.setHost (fromString host) (Warp.setPort port Warp.defaultSettings)
    hPutStrLn stderr ("hydrometer serving on " <> host <> ":" <> show port)
    mapConcurrently_ id $
      [ Warp.runSettings settings (app env)
      , runMaintenance d (fromMaybe 14 (retentionDays cfg)) ]
      ++ [ runQueuePoller d mgr qr queue | Just qr <- [queueRunner cfg] ]
      ++ [ runAgentPoller d mgr pollSec agentStates a | a <- agents cfg ]
  ("mcp", []) -> do
    d <- openDb (fromMaybe (db cfg) (readOnlyDb cfg)) 1
    let (host, port) = listenAddr cfg
        h = if host `elem` ["*", "0.0.0.0", "::", ""] then "127.0.0.1" else host
        hostPart = if ':' `elem` h then "[" <> h <> "]" else h
    runMcp (T.pack ("http://" <> hostPart <> ":" <> show port)) d
  ("query", [sql]) -> do
    d <- openDb (fromMaybe (db cfg) (readOnlyDb cfg)) 1
    t <- withConn d (\c -> queryTableReadOnly c 60000 10000 (T.pack sql))
    BLC.putStrLn (A.encode t)
  ("ingest", [machine, file]) -> do
    d <- openDb (db cfg) 1
    sr <- either fail pure =<< A.eitherDecodeFileStrict file
    ingest d (T.pack machine) ("file:" <> T.pack file :: Text) sr
    putStrLn "ingested"
  _ -> hPutStrLn stderr usage >> exitFailure
