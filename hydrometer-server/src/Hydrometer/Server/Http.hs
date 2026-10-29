-- | The HTTP API and the static UI.
--
-- All API responses are JSON.  Times in query parameters are epoch
-- milliseconds (@from@, @to@); the default range is the last 6 hours.
module Hydrometer.Server.Http
  ( Env (..)
  , app
  , apiIndex
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Types
import Network.Wai
import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, takeFileName, (</>))
import System.IO (IOMode (..), withBinaryFile)
import Text.Read (readMaybe)

import Hydrometer.Api (CacheNow)
import Hydrometer.Server.Blockers (BlockerEnv, evalBlockers)
import Hydrometer.Server.Db
import Hydrometer.Server.Ingest (AgentState)
import Hydrometer.Server.Now (buildNow)
import Hydrometer.Server.Queries
import Hydrometer.Server.QueueRunner (QueueState)
import Hydrometer.Server.StepGraph (GraphEnv, evalGraph)
import Hydrometer.Server.Substitutions (SubstEnv, evalSubstitutions)

data Env = Env
  { envDb     :: Db
  , envAgents :: TVar (Map Text AgentState)
  , envQueue  :: TVar QueueState
  , envStatic :: Maybe FilePath
  , envBlockers :: BlockerEnv
  , envGraph  :: GraphEnv
  , envSubst  :: SubstEnv
  , envCache  :: TVar (Maybe CacheNow)
  , envEvalSlots :: Maybe Int
  }

-- | A machine-readable description of the endpoints (served at /api).
apiIndex :: A.Value
apiIndex = A.object
  [ "description" .= ("hydrometer API. Times are epoch milliseconds; from/to default to the last 6 hours. Tables are {columns, rows}; series are {time, stepMs, series: {name: [values]}}." :: Text)
  , "endpoints" .= A.object
      [ "GET /api/now" .= ("Every machine: host CPU/memory/network now, the steps running in each slot with their live CPU and memory, queue depth per system, running evaluations, alerts (e.g. a stalled queue runner)." :: Text)
      , "GET /api/machine/{name}/series?from&to" .= ("Host metrics over time: cpu_util (0..1), mem_frac, mem_pressure, net/disk rates (bytes/s), swap, pageouts, open files." :: Text)
      , "GET /api/machine/{name}/steps?from&to" .= ("Build steps that ran on the machine in the range (Gantt data) with their CPU/memory." :: Text)
      , "GET /api/top?group&metric&from&to&limit" .= ("Heaviest groups of finished steps. group: package, component_kind, component, platform, job_compiler, project, jobset, job, drv_name, system, machine, run_kind. metric: cpu, wall, rss, net, steps, import." :: Text)
      , "GET /api/bottlenecks?from&to" .= ("Machine-minutes per limiting factor (cpu, memory, network, slots, scheduler, starved, idle) over time, plus a per-machine summary." :: Text)
      , "GET /api/farm?from&to" .= ("Farm-wide CPU utilisation, network, concurrent builds and runnable queue depth over time." :: Text)
      , "GET /api/evals?limit" .= ("Recent evaluations with build progress, and the jobsets being evaluated now." :: Text)
      , "GET /api/eval/{id}" .= ("One evaluation: summary (time split into import/build/upload, CPU), timeline of running steps vs unfinished builds, and its longest steps." :: Text)
      , "GET /api/eval/{id}/blockers" .= ("What an evaluation's unfinished builds wait on: counts by state, and the steps holding the most builds, with why runnable ones are not dispatched." :: Text)
      , "GET /api/eval/{id}/graph" .= ("The steps of an evaluation (the 300 longest) and which of them each waited for, from the .drv files: nodes with start/stop, kind (built or substituted), status; edges [dependency, dependent] as node indexes." :: Text)
      , "GET /api/eval/{id}/substitutions" .= ("An evaluation's substituted steps (the 300 longest): where the outputs came from (a LAN peer via peernix, or the first public cache that has them), what was downloaded (closure paths registered during the step, NAR bytes, by source), and output and closure sizes." :: Text)
      , "GET /api/build/{id}" .= ("The steps of one build with their resource use." :: Text)
      , "GET /api/step/{build}/{stepnr}/series" .= ("CPU cores, memory and network of one step over its life." :: Text)
      ]
  ]

app :: Env -> Application
app env req respond = do
  r <- try (route (pathInfo req))
  case r of
    Right resp -> respond resp
    Left (e :: SomeException) -> respond (jsonStatus status500 (A.object ["error" .= displayException e]))
  where
    db = envDb env
    q k = case lookup k (queryString req) of
      Just (Just v) -> Just (T.decodeUtf8 v)
      _ -> Nothing
    qInt :: Read a => BC.ByteString -> Maybe a
    qInt k = q k >>= readMaybe . T.unpack
    range = do
      now <- getCurrentTime
      let to = maybe now fromEpochMs (qInt "to" :: Maybe Int64)
          -- A negative from is relative: that many ms before to.
          from = case qInt "from" :: Maybe Int64 of
            Just ms | ms < 0 -> addUTCTime (fromIntegral ms / 1000) to
                    | otherwise -> fromEpochMs ms
            Nothing -> addUTCTime (-6 * 3600) to
      pure (TimeRange from to)
    ok :: A.ToJSON a => a -> IO Response
    ok = pure . jsonStatus status200

    route = \case
      ["api"] -> ok apiIndex
      ["api", "now"] -> withConn db (\c -> buildNow c (envAgents env) (envQueue env) (envBlockers env) (envCache env) (envEvalSlots env)) >>= ok
      ["api", "machine", m, "series"] -> range >>= \tr -> withConn db (\c -> machineSeries c m tr) >>= ok
      ["api", "machine", m, "steps"] -> range >>= \tr -> withConn db (\c -> machineSteps c m tr) >>= ok
      ["api", "top"] -> range >>= \tr -> withConn db (\c ->
          topJobs c (maybe "package" id (q "group")) (maybe "cpu" id (q "metric")) tr (maybe 50 id (qInt "limit"))) >>= ok
      ["api", "bottlenecks"] -> range >>= \tr -> withConn db (\c -> do
          s <- bottleneckSeries c tr
          t <- bottleneckSummary c tr
          pure (A.object ["series" .= s, "summary" .= t])) >>= ok
      ["api", "farm"] -> range >>= \tr -> withConn db (\c -> farmSeries c tr) >>= ok
      ["api", "evals"] -> withConn db (\c -> do
          t <- recentEvals c (maybe 50 id (qInt "limit"))
          running <- runningEvals c
          pure (A.object
            [ "evals" .= t
            , "running" .= [ A.object ["project" .= p, "jobset" .= j, "started" .= s, "previousEval" .= e]
                           | (p, j, s, e, _) <- running ] ])) >>= ok
      ["api", "eval", i] | Just eid <- readMaybe (T.unpack i) -> withConn db (\c -> do
          s <- evalSummary c eid
          tl <- evalTimeline c eid
          st <- evalSteps c eid 200
          pure (A.object ["summary" .= s, "timeline" .= tl, "steps" .= st])) >>= ok
      ["api", "eval", i, "blockers"] | Just eid <- readMaybe (T.unpack i) -> do
          qs <- readTVarIO (envQueue env)
          withConn db (\c -> evalBlockers (envBlockers env) c qs eid) >>= ok
      ["api", "eval", i, "graph"] | Just eid <- readMaybe (T.unpack i) -> withConn db (\c -> evalGraph (envGraph env) c eid) >>= ok
      ["api", "eval", i, "substitutions"] | Just eid <- readMaybe (T.unpack i) -> withConn db (\c -> evalSubstitutions (envSubst env) c eid) >>= ok
      ["api", "build", i] | Just bid <- readMaybe (T.unpack i) -> withConn db (\c -> buildSteps c bid) >>= ok
      ["api", "step", b, s, "series"] | Just bid <- readMaybe (T.unpack b), Just sn <- readMaybe (T.unpack s) ->
          withConn db (\c -> stepSeries c bid sn) >>= ok
      ("api" : _) -> pure (jsonStatus status404 (A.object ["error" .= ("no such endpoint; see /api" :: Text)]))
      path -> staticFile path

    staticFile path = case envStatic env of
      Nothing -> pure (responseLBS status404 [] "no UI configured\n")
      Just dir -> do
        let rel = if null path then "index.html" else T.unpack (T.intercalate "/" path)
        if ".." `elem` path then pure (responseLBS status400 [] "bad path\n") else do
          exists <- doesFileExist (dir </> rel)
          -- Unknown paths fall back to index.html (the UI routes on the hash).
          let file = if exists then dir </> rel else dir </> "index.html"
              -- Files in the nix store all have mtime 1970, so Last-Modified
              -- revalidation would answer 304 across deploys (and Cloudflare
              -- kept serving the old UI).  The site's store path names the
              -- build, so it is the ETag; and the file is streamed rather
              -- than sent with responseFile, whose If-Modified-Since check
              -- would still answer 304 to a cache that has no ETag yet.
              etag = BC.pack ("\"" <> takeFileName (dropTrailingPathSeparator dir) <> "\"")
              hdrs = [(hContentType, contentType file), (hCacheControl, "no-cache"), (hETag, etag)]
          pure $ if lookup hIfNoneMatch (requestHeaders req) == Just etag
            then responseLBS status304 hdrs ""
            else responseStream status200 hdrs $ \send flush ->
              withBinaryFile file ReadMode $ \h ->
                let loop = do
                      chunk <- BS.hGetSome h 65536
                      unless (BS.null chunk) (send (byteString chunk) >> loop)
                in loop >> flush

    contentType f
      | ".html" `isSuffixOf` f = "text/html; charset=utf-8"
      | ".js" `isSuffixOf` f = "application/javascript"
      | ".css" `isSuffixOf` f = "text/css"
      | ".svg" `isSuffixOf` f = "image/svg+xml"
      | otherwise = "application/octet-stream"

jsonStatus :: A.ToJSON a => Status -> a -> Response
jsonStatus st v = responseLBS st [(hContentType, "application/json"), (hAccessControlAllowOrigin, "*")] (A.encode v)
  where hAccessControlAllowOrigin = "Access-Control-Allow-Origin"

