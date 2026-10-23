-- | A Model Context Protocol server on stdio, so AI agents can ask
-- hydrometer questions directly:
--
-- > ssh x86_64-linux-0 hydrometer mcp
--
-- Tools that need the live in-memory state (now) go through the running
-- server's HTTP API; @sql@ and @schema@ use a read-only database
-- connection.
module Hydrometer.Server.Mcp
  ( runMcp
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Client
import System.IO

import Hydrometer.Server.Db

data McpEnv = McpEnv
  { meApi :: Text
  , meDb  :: Db
  , meMgr :: Manager
  }

instructions :: Text
instructions = T.unlines
  [ "hydrometer measures the ci.zw3rk.com Hydra build farm: CPU, memory, network and disk of every"
  , "macOS builder (and linux-0), attributed to the hydra build step or evaluation that used them."
  , "Start with `now` for the current state, `top_jobs` for what is expensive, `bottlenecks` to see"
  , "whether the farm is CPU/memory/network/slot/scheduler limited, and `explain_eval` for why an"
  , "evaluation (a PR) is slow. `schema` lists the SQL views (hydrometer.v_*) with documentation;"
  , "`sql` runs read-only SQL against them and against hydra's own tables (builds, buildsteps,"
  , "jobsets, jobsetevals, jobsetevalmembers). Times in arguments are hours back from now."
  ]

tools :: [A.Value]
tools =
  [ tool "now" "What every machine is doing right now: host CPU/memory/network, the build in each slot with its live CPU and memory, runnable queue depth per system, running evaluations, and alerts (e.g. a stalled queue runner). A slot marked orphaned is a step hydra lists as running that no builder is running." []
  , tool "top_jobs" "Which kinds of job use the most resources. Groups finished steps and sums CPU hours, wall hours, peak memory and network."
      [ ("group", "string", "package (default), component_kind, component, platform, job_compiler, project, jobset, job, drv_name, system, machine or run_kind")
      , ("metric", "string", "cpu (default), wall, rss, net, steps or import")
      , ("hours", "number", "how far back to look (default 24)")
      , ("limit", "number", "rows (default 30)") ]
  , tool "bottlenecks" "Whether the farm is CPU-, memory-, network-, slot- or scheduler-bound: machine-minutes per limiting factor, per machine."
      [ ("hours", "number", "how far back to look (default 24)") ]
  , tool "machine" "One machine's recent history: host metrics summary and the steps it ran."
      [ ("name", "string", "machine name, e.g. aarch64-darwin-9"), ("hours", "number", "default 2") ]
  , tool "recent_evals" "Recent hydra evaluations with build progress, and jobsets being evaluated now."
      [ ("limit", "number", "default 20") ]
  , tool "explain_eval" "Why an evaluation (e.g. a PR) is taking long: time split into import/build/upload, CPU used, timeline of running steps vs unfinished builds (gaps = waiting for machines or dependencies), and the longest steps."
      [ ("eval_id", "number", "hydra jobsetevals.id") ]
  , tool "explain_build" "The steps of one hydra build with their resource use."
      [ ("build_id", "number", "hydra builds.id") ]
  , tool "schema" "The documented hydrometer views and tables, with column names and descriptions. Read this before writing SQL." []
  , tool "sql" "Run a read-only SQL query (PostgreSQL) and get {columns, rows}. 30 s timeout, at most 500 rows. Prefer the hydrometer.v_* views; filter hydra's buildsteps on stoptime (it is indexed) or build."
      [ ("query", "string", "a single SELECT statement") ]
  ]
  where
    tool :: Text -> Text -> [(Text, Text, Text)] -> A.Value
    tool n d ps = A.object
      [ "name" .= n, "description" .= d
      , "inputSchema" .= A.object
          [ "type" .= ("object" :: Text)
          , "properties" .= A.object [ K.fromText p .= A.object ["type" .= ty, "description" .= desc] | (p, ty, desc) <- ps ]
          , "required" .= [ p | (p, _, _) <- ps, p `elem` ["name", "eval_id", "build_id", "query"] ]
          ] ]

runMcp :: Text -> Db -> IO ()
runMcp api d = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  mgr <- newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 60000000 }
  let env = McpEnv api d mgr
      loop = do
        eof <- isEOF
        unless eof $ do
          line <- BC.getLine
          unless (BC.null line) $ case A.decodeStrict line of
            Nothing -> send (rpcError A.Null (-32700) "parse error")
            Just msg -> handle env msg
          loop
  loop

send :: A.Value -> IO ()
send v = BLC.putStrLn (A.encode v)

rpcError :: A.Value -> Int -> Text -> A.Value
rpcError i code m = A.object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "error" .= A.object ["code" .= code, "message" .= m]]

handle :: McpEnv -> A.Value -> IO ()
handle env msg = case A.parseMaybe parseMsg msg of
  Nothing -> send (rpcError A.Null (-32600) "invalid request")
  Just (mid, method, params) -> case (mid, method) of
    (Nothing, _) -> pure ()   -- notifications (notifications/initialized, cancelled)
    (Just i, "initialize") -> reply i $ A.object
      [ "protocolVersion" .= fromMaybe ("2025-06-18" :: Text) (A.parseMaybe (A.withObject "p" (.: "protocolVersion")) params)
      , "capabilities" .= A.object ["tools" .= A.object []]
      , "serverInfo" .= A.object ["name" .= ("hydrometer" :: Text), "version" .= ("0.1.0.0" :: Text)]
      , "instructions" .= instructions ]
    (Just i, "ping") -> reply i (A.object [])
    (Just i, "tools/list") -> reply i (A.object ["tools" .= tools])
    (Just i, "tools/call") -> do
      let call = A.parseMaybe (A.withObject "call" $ \o -> (,) <$> o .: "name" <*> (fromMaybe (A.object []) <$> o .:? "arguments")) params
      case call of
        Nothing -> send (rpcError i (-32602) "invalid params")
        Just (name, args) -> do
          r <- try (callTool env name args)
          let (txt, isErr) = case r of
                Right v -> (T.decodeUtf8 (BC.toStrict (A.encode v)), False)
                Left (e :: SomeException) -> (T.pack (displayException e), True)
          reply i $ A.object
            [ "content" .= [A.object ["type" .= ("text" :: Text), "text" .= txt]]
            , "isError" .= isErr ]
    (Just i, _) -> send (rpcError i (-32601) ("method not found: " <> method))
  where
    reply i r = send (A.object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= r])
    parseMsg = A.withObject "msg" $ \o -> (,,) <$> o .:? "id" <*> o .: "method" <*> (fromMaybe A.Null <$> o .:? "params")

callTool :: McpEnv -> Text -> A.Value -> IO A.Value
callTool env name args = case name of
  "now" -> api "/api/now" []
  "top_jobs" -> rangeArgs 24 >>= \r -> api "/api/top" (r ++
      [("group", str "group" "package"), ("metric", str "metric" "cpu"), ("limit", T.pack (show (num "limit" 30)))])
  "bottlenecks" -> rangeArgs 24 >>= \r -> do
      v <- api "/api/bottlenecks" r
      -- The per-machine summary answers the question; the series is for charts.
      pure (maybe v id (A.parseMaybe (A.withObject "b" (.: "summary")) v))
  "machine" -> do
      r <- rangeArgs 2
      let m = str "name" ""
      series <- api ("/api/machine/" <> m <> "/series") r
      steps <- api ("/api/machine/" <> m <> "/steps") r
      pure (A.object ["series_summary" .= summarise series, "steps" .= steps])
  "recent_evals" -> api "/api/evals" [("limit", T.pack (show (num "limit" 20)))]
  "explain_eval" -> api ("/api/eval/" <> T.pack (show (num "eval_id" 0))) []
  "explain_build" -> api ("/api/build/" <> T.pack (show (num "build_id" 0))) []
  "schema" -> A.toJSON <$> withConn (meDb env) (\c -> queryTableReadOnly c 30000 2000 schemaSql)
  "sql" -> A.toJSON <$> withConn (meDb env) (\c -> queryTableReadOnly c 30000 500 (str "query" ""))
  _ -> fail ("unknown tool " <> T.unpack name)
  where
    str k def = fromMaybe def (A.parseMaybe (A.withObject "args" (.: K.fromText k)) args)
    num :: Text -> Int -> Int
    num k def = maybe def (round :: Double -> Int) (A.parseMaybe (A.withObject "args" (.: K.fromText k)) args)
    rangeArgs :: Double -> IO [(Text, Text)]
    rangeArgs defHours = do
      now <- getCurrentTime
      let hours = fromMaybe defHours (A.parseMaybe (A.withObject "args" (.: "hours")) args)
          from = addUTCTime (realToFrac (negate hours * 3600)) now
      pure [("from", T.pack (show (epochMs from))), ("to", T.pack (show (epochMs now)))]
    api :: Text -> [(Text, Text)] -> IO A.Value
    api path qs = do
      req <- parseRequest (T.unpack (meApi env <> path))
      let req' = setQueryString [ (T.encodeUtf8 k, Just (T.encodeUtf8 v)) | (k, v) <- qs ] req
      resp <- httpLbs req' (meMgr env)
      either fail pure (A.eitherDecode (responseBody resp))

-- | min/avg/max of each series column, so an agent gets the gist without
-- hundreds of points.
summarise :: A.Value -> A.Value
summarise v = fromMaybe v $ flip A.parseMaybe v $ A.withObject "series" $ \o -> do
  cols <- o .: "series" :: A.Parser (KM.KeyMap [Maybe Double])
  pure $ A.object
    [ k .= A.object ["min" .= minimum xs, "avg" .= (sum xs / fromIntegral (length xs)), "max" .= maximum xs, "points" .= length xs]
    | (k, vs) <- toListKM cols, let xs = [ x | Just x <- vs ], not (null xs) ]
  where toListKM = KM.toList

schemaSql :: Text
schemaSql = T.unlines
  [ "SELECT c.relname AS name, CASE c.relkind WHEN 'v' THEN 'view' ELSE 'table' END AS kind,"
  , "  obj_description(c.oid) AS description,"
  , "  (SELECT string_agg(a.attname || ' ' || format_type(a.atttypid, a.atttypmod)"
  , "     || coalesce(' -- ' || col_description(c.oid, a.attnum), ''), ', ' ORDER BY a.attnum)"
  , "   FROM pg_attribute a WHERE a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped) AS columns"
  , "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace"
  , "WHERE n.nspname = 'hydrometer' AND c.relkind IN ('v', 'r', 'p') AND NOT c.relispartition"
  , "ORDER BY c.relkind DESC, c.relname"
  ]

