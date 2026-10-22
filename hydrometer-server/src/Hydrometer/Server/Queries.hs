-- | The canned queries behind the API and the MCP tools.  Each answers one
-- of the questions hydrometer exists for; the SQL is written against the
-- documented views so it doubles as an example for people writing their own.
module Hydrometer.Server.Queries
  ( TimeRange (..)
  , groupColumns
  , machineSeries
  , machineSteps
  , topJobs
  , bottleneckSeries
  , bottleneckSummary
  , farmSeries
  , recentEvals
  , evalSummary
  , evalSteps
  , evalTimeline
  , buildSteps
  , stepSeries
  , busySteps
  , lastStepFinish
  , runningEvals
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import Data.String (fromString)

import Hydrometer.Api (Series, Table (..))
import Hydrometer.Server.Db

data TimeRange = TimeRange
  { trFrom :: !UTCTime
  , trTo   :: !UTCTime
  }

-- | Grouping keys accepted by 'topJobs' (columns of v_step_usage).
groupColumns :: [Text]
groupColumns =
  [ "package", "component_kind", "component", "platform", "job_compiler"
  , "project", "jobset", "job", "drv_name", "system", "machine", "run_kind" ]

-- | Bucket size for a range: about 400 points.
bucketSeconds :: TimeRange -> Int64
bucketSeconds (TimeRange f t) =
  let secs = max 60 (round (diffUTCTime t f) :: Int64)
  in case [ b | b <- [5, 10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200, 21600], secs `div` b <= 400 ] of
       (b : _) -> b
       [] -> 86400

machineSeries :: Connection -> Text -> TimeRange -> IO Series
machineSeries c machine tr@(TimeRange f t) = do
  let b = bucketSeconds tr
  tbl <- if b < 60
    then queryTable c
      "SELECT (extract(epoch FROM date_bin(make_interval(secs => ?), t, '2000-01-01')) * 1000)::bigint AS t, \
      \  avg(cpu_util) AS cpu_util, max(mem_frac) AS mem_frac, max(mem_pressure) AS mem_pressure, \
      \  avg(net_rx_rate) AS net_rx_rate, avg(net_tx_rate) AS net_tx_rate, \
      \  avg(disk_read_rate) AS disk_read_rate, avg(disk_write_rate) AS disk_write_rate, \
      \  max(swap_used) AS swap_used, sum(pageouts) AS pageouts, max(open_files) AS open_files \
      \FROM hydrometer.v_host WHERE machine = ? AND t >= ? AND t < ? GROUP BY 1 ORDER BY 1"
      (b, machine, f, t)
    else queryTable c
      "SELECT (extract(epoch FROM date_bin(make_interval(secs => ?), t, '2000-01-01')) * 1000)::bigint AS t, \
      \  avg(cpu_util) AS cpu_util, max(mem_used_max::float8 / nullif(mem_total, 0)) AS mem_frac, \
      \  max(mem_pressure_max) AS mem_pressure, avg(net_rx_rate) AS net_rx_rate, avg(net_tx_rate) AS net_tx_rate, \
      \  avg(disk_read_rate) AS disk_read_rate, avg(disk_write_rate) AS disk_write_rate, \
      \  max(swap_used_max) AS swap_used, sum(pageouts) AS pageouts, max(open_files_max) AS open_files, \
      \  max(runs_max) AS runs \
      \FROM hydrometer.v_machine_minute WHERE machine = ? AND t >= ? AND t < ? GROUP BY 1 ORDER BY 1"
      (b, machine, f, t)
  pure (tableToSeries (b * 1000) tbl)

-- | Steps that overlapped the range on a machine (for a Gantt chart).
-- buildsteps is only indexed on stoptime, so bound that first.
machineSteps :: Connection -> Text -> TimeRange -> IO Table
machineSteps c machine (TimeRange f t) = queryTable c
  "SELECT build, stepnr, project, jobset, job, drv_name, package, component_kind, platform, system, \
  \  (extract(epoch FROM started) * 1000)::bigint AS started, \
  \  (extract(epoch FROM coalesce(stopped, now())) * 1000)::bigint AS stopped, \
  \  status, busy, import_s, build_s, upload_s, run_kind, cpu_s, avg_cores, peak_rss, net_rx_bytes \
  \FROM hydrometer.v_step_usage s \
  \WHERE machine = ? AND started < ? \
  \  AND ((stopped >= ? AND s.status IS NOT NULL) OR (busy <> 0 AND stopped IS NULL)) \
  \ORDER BY started"
  (machine, t, f)

-- | Heaviest groups of steps that finished in the range.
topJobs :: Connection -> Text -> Text -> TimeRange -> Int -> IO Table
topJobs c groupCol metric (TimeRange f t) limit = do
  let col = if groupCol `elem` groupColumns then groupCol else "package"
      order = case metric of
        "wall" -> "wall_h"
        "rss" -> "max_peak_rss"
        "net" -> "net_rx_gb"
        "steps" -> "steps"
        "import" -> "import_h"
        _ -> "cpu_h"
  queryTable c (Query (fromString (
    "SELECT " <> quoteIdent col <> " AS key, count(*) AS steps, count(cpu_s) AS measured_steps, \
    \  round((sum(cpu_s) / 3600)::numeric, 2) AS cpu_h, \
    \  round((sum(wall_s) / 3600.0)::numeric, 2) AS wall_h, \
    \  round((sum(cpu_s) / nullif(sum(wall_s) FILTER (WHERE cpu_s IS NOT NULL), 0))::numeric, 2) AS avg_cores, \
    \  max(peak_rss) AS max_peak_rss, \
    \  round((sum(net_rx_bytes) / 1e9)::numeric, 2) AS net_rx_gb, \
    \  round((sum(import_s) / 3600)::numeric, 2) AS import_h, \
    \  round((sum(build_s) / 3600)::numeric, 2) AS build_h, \
    \  count(*) FILTER (WHERE status <> 0) AS failed \
    \FROM hydrometer.v_step_usage \
    \WHERE stopped >= ? AND stopped < ? AND status IS NOT NULL \
    \GROUP BY 1 ORDER BY " <> order <> " DESC NULLS LAST, wall_h DESC LIMIT ?")))
    (f, t, limit)
  where
    quoteIdent x = "\"" <> T.unpack (T.replace "\"" "\"\"" x) <> "\""

-- | Machine-minutes per bottleneck class, bucketed over the range.
bottleneckSeries :: Connection -> TimeRange -> IO Series
bottleneckSeries c tr@(TimeRange f t) = do
  let b = max 60 (bucketSeconds tr)
  tbl <- queryTable c
    "SELECT (extract(epoch FROM date_bin(make_interval(secs => ?), t, '2000-01-01')) * 1000)::bigint AS t, \
    \  count(*) FILTER (WHERE bottleneck = 'cpu') AS cpu, \
    \  count(*) FILTER (WHERE bottleneck = 'memory') AS memory, \
    \  count(*) FILTER (WHERE bottleneck = 'network') AS network, \
    \  count(*) FILTER (WHERE bottleneck = 'slots') AS slots, \
    \  count(*) FILTER (WHERE bottleneck = 'scheduler') AS scheduler, \
    \  count(*) FILTER (WHERE bottleneck = 'starved') AS starved, \
    \  count(*) FILTER (WHERE bottleneck = 'idle') AS idle, \
    \  count(*) FILTER (WHERE bottleneck = 'disabled') AS disabled \
    \FROM hydrometer.v_bottleneck_minute WHERE t >= ? AND t < ? GROUP BY 1 ORDER BY 1"
    (b, f, t)
  pure (tableToSeries (b * 1000) tbl)

bottleneckSummary :: Connection -> TimeRange -> IO Table
bottleneckSummary c (TimeRange f t) = queryTable c
  "SELECT machine, count(*) AS minutes, \
  \  round(avg(cpu_util)::numeric, 3) AS avg_cpu_util, \
  \  count(*) FILTER (WHERE bottleneck = 'cpu') AS cpu, \
  \  count(*) FILTER (WHERE bottleneck = 'memory') AS memory, \
  \  count(*) FILTER (WHERE bottleneck = 'network') AS network, \
  \  count(*) FILTER (WHERE bottleneck = 'slots') AS slots, \
  \  count(*) FILTER (WHERE bottleneck = 'scheduler') AS scheduler, \
  \  count(*) FILTER (WHERE bottleneck = 'starved') AS starved, \
  \  count(*) FILTER (WHERE bottleneck = 'idle') AS idle \
  \FROM hydrometer.v_bottleneck_minute WHERE t >= ? AND t < ? GROUP BY ROLLUP (machine) ORDER BY machine NULLS LAST"
  (f, t)

-- | Farm-wide utilisation and queue depth.
farmSeries :: Connection -> TimeRange -> IO Series
farmSeries c tr@(TimeRange f t) = do
  let b = max 60 (bucketSeconds tr)
  tbl <- queryTable c
    "WITH h AS ( \
    \  SELECT date_bin(make_interval(secs => ?), t, '2000-01-01') AS t, \
    \    sum(cpu_busy_ms)::float8 / nullif(sum(cpu_total_ms), 0) AS cpu_util, \
    \    sum(net_rx_bytes)::float8 / ? AS net_rx_rate, sum(runs_max) / greatest(count(DISTINCT t), 1)::float8 AS runs \
    \  FROM hydrometer.host_minute WHERE t >= ? AND t < ? GROUP BY 1 \
    \), q AS ( \
    \  SELECT date_bin(make_interval(secs => ?), t, '2000-01-01') AS t, \
    \    avg(runnable) FILTER (WHERE system LIKE '%-linux') * count(DISTINCT system) FILTER (WHERE system LIKE '%-linux') AS runnable_linux, \
    \    avg(runnable) FILTER (WHERE system LIKE '%-darwin') * count(DISTINCT system) FILTER (WHERE system LIKE '%-darwin') AS runnable_darwin \
    \  FROM hydrometer.queue_samples WHERE t >= ? AND t < ? GROUP BY 1 \
    \) \
    \SELECT (extract(epoch FROM coalesce(h.t, q.t)) * 1000)::bigint AS t, h.cpu_util, h.net_rx_rate, h.runs, \
    \  q.runnable_linux, q.runnable_darwin \
    \FROM h FULL JOIN q ON q.t = h.t ORDER BY 1"
    (b, b, f, t, b, f, t)
  pure (tableToSeries (b * 1000) tbl)

recentEvals :: Connection -> Int -> IO Table
recentEvals c limit = queryTable c
  "WITH e AS (SELECT id FROM jobsetevals ORDER BY id DESC LIMIT ?) \
  \SELECT p.eval_id, p.project, p.jobset, (extract(epoch FROM p.evaluated) * 1000)::bigint AS evaluated, \
  \  p.eval_s, p.builds, p.finished, p.succeeded, p.failed, p.queued, \
  \  (extract(epoch FROM p.last_finish) * 1000)::bigint AS last_finish \
  \FROM hydrometer.v_eval_progress p WHERE p.eval_id IN (SELECT id FROM e) ORDER BY p.eval_id DESC"
  (Only limit)

evalSummary :: Connection -> Int -> IO Table
evalSummary c evalId = queryTable c
  "WITH steps AS ( \
  \  SELECT s.* FROM hydrometer.v_step_usage s \
  \  WHERE s.build IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) \
  \) \
  \SELECT p.eval_id, p.project, p.jobset, (extract(epoch FROM p.evaluated) * 1000)::bigint AS evaluated, p.eval_s, \
  \  p.builds, p.finished, p.succeeded, p.failed, p.queued, \
  \  (extract(epoch FROM p.last_finish) * 1000)::bigint AS last_finish, \
  \  (SELECT count(*) FROM steps) AS steps, \
  \  (SELECT count(*) FROM steps WHERE busy <> 0 AND stopped IS NULL) AS running_steps, \
  \  (SELECT round((sum(wall_s) / 3600.0)::numeric, 2) FROM steps) AS step_wall_h, \
  \  (SELECT round((sum(import_s) / 3600)::numeric, 2) FROM steps) AS import_h, \
  \  (SELECT round((sum(build_s) / 3600)::numeric, 2) FROM steps) AS build_h, \
  \  (SELECT round((sum(upload_s) / 3600)::numeric, 2) FROM steps) AS upload_h, \
  \  (SELECT round((sum(cpu_s) / 3600)::numeric, 2) FROM steps) AS cpu_h, \
  \  (SELECT (extract(epoch FROM min(started)) * 1000)::bigint FROM steps) AS first_step, \
  \  (SELECT round((extract(epoch FROM coalesce(max(stopped), now()) - min(started)) / 3600)::numeric, 2) FROM steps) AS span_h \
  \FROM hydrometer.v_eval_progress p WHERE p.eval_id = ?"
  (evalId, evalId)

-- | The steps an evaluation's builds ran, longest first.
evalSteps :: Connection -> Int -> Int -> IO Table
evalSteps c evalId limit = queryTable c
  "SELECT s.build, s.stepnr, s.job, s.drv_name, s.package, s.platform, s.system, s.machine, \
  \  (extract(epoch FROM s.started) * 1000)::bigint AS started, \
  \  (extract(epoch FROM s.stopped) * 1000)::bigint AS stopped, \
  \  coalesce(s.wall_s, extract(epoch FROM now() - s.started)::int) AS wall_s, \
  \  s.status, s.busy, s.import_s, s.build_s, s.upload_s, s.cpu_s, s.avg_cores, s.peak_rss \
  \FROM hydrometer.v_step_usage s \
  \WHERE s.build IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) \
  \ORDER BY wall_s DESC NULLS LAST LIMIT ?"
  (evalId, limit)

-- | Over the life of an evaluation: how many of its steps were running and
-- how many of its builds were still unfinished, per bucket.  Gaps where
-- builds remain but nothing runs are waits on machines or other jobsets.
evalTimeline :: Connection -> Int -> IO Series
evalTimeline c evalId = do
  tbl <- queryTable c
    "WITH steps AS ( \
    \  SELECT s.starttime, coalesce(s.stoptime, extract(epoch FROM now())::int) AS stoptime, s.system, s.machine \
    \  FROM buildsteps s WHERE s.build IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) AND s.starttime IS NOT NULL \
    \), builds AS ( \
    \  SELECT b.timestamp, coalesce(b.stoptime, extract(epoch FROM now())::int) AS stoptime FROM builds b \
    \  WHERE b.id IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) \
    \), span AS ( \
    \  SELECT least((SELECT min(starttime) FROM steps), (SELECT timestamp FROM jobsetevals WHERE id = ?)) AS lo, \
    \    greatest((SELECT max(stoptime) FROM steps), (SELECT timestamp FROM jobsetevals WHERE id = ?) + 60) AS hi \
    \), buckets AS ( \
    \  SELECT generate_series(lo, hi, greatest((hi - lo) / 300, 30)) AS t, greatest((hi - lo) / 300, 30) AS w FROM span \
    \) \
    \SELECT (bk.t::bigint * 1000) AS t, \
    \  (SELECT count(*) FROM steps s WHERE s.starttime < bk.t + bk.w AND s.stoptime > bk.t) AS running_steps, \
    \  (SELECT count(*) FROM steps s WHERE s.starttime < bk.t + bk.w AND s.stoptime > bk.t AND s.system LIKE '%-linux') AS running_linux, \
    \  (SELECT count(*) FROM steps s WHERE s.starttime < bk.t + bk.w AND s.stoptime > bk.t AND s.system LIKE '%-darwin') AS running_darwin, \
    \  (SELECT count(*) FROM builds b WHERE b.stoptime > bk.t) AS unfinished_builds \
    \FROM buckets bk ORDER BY 1"
    (evalId, evalId, evalId, evalId)
  pure (tableToSeries 0 tbl)

buildSteps :: Connection -> Int -> IO Table
buildSteps c build = queryTable c
  "SELECT s.stepnr, s.drv_name, s.package, s.component_kind, s.platform, s.system, s.machine, \
  \  (extract(epoch FROM s.started) * 1000)::bigint AS started, \
  \  (extract(epoch FROM s.stopped) * 1000)::bigint AS stopped, s.wall_s, s.status, s.busy, \
  \  s.import_s, s.build_s, s.upload_s, s.run_kind, s.cpu_s, s.avg_cores, s.peak_rss, s.net_rx_bytes \
  \FROM hydrometer.v_step_usage s WHERE s.build = ? ORDER BY s.stepnr"
  (Only build)

-- | Resource use over the life of one step (from its run's samples).
stepSeries :: Connection -> Int -> Int -> IO Series
stepSeries c build stepnr = do
  tbl <- queryTable c
    "SELECT (extract(epoch FROM rs.t) * 1000)::bigint AS t, \
    \  rs.cpu_ms::float8 / 5000 AS cores, rs.footprint, rs.rss, \
    \  rs.net_rx_bytes / 5.0 AS net_rx_rate, rs.disk_write_bytes / 5.0 AS disk_write_rate, rs.procs \
    \FROM hydrometer.runs r JOIN hydrometer.run_samples rs ON rs.run_id = r.id \
    \WHERE r.build = ? AND r.stepnr = ? ORDER BY 1"
    (build, stepnr)
  pure (tableToSeries 5000 tbl)

-- | Steps hydra says are running now.
busySteps :: Connection -> IO [(Text, Int, Int, Text, Text, Text, Text, Text, Maybe Text, Maybe Int64)]
busySteps c = query_ c
  "SELECT s.machine, s.build, s.stepnr, j.project, j.name, b.job, hydrometer.drv_name(s.drvpath), s.drvpath, s.system, \
  \  s.starttime::bigint * 1000 \
  \FROM buildsteps s JOIN builds b ON b.id = s.build JOIN jobsets j ON j.id = b.jobset_id \
  \WHERE s.busy <> 0 AND b.finished = 0"

-- | When the most recent build step finished (epoch ms).
lastStepFinish :: Connection -> IO (Maybe Int64)
lastStepFinish c = do
  rs <- query_ c
    "SELECT max(stoptime)::bigint * 1000 FROM (SELECT stoptime FROM buildsteps \
    \ WHERE starttime IS NOT NULL AND stoptime IS NOT NULL ORDER BY stoptime DESC LIMIT 1) x"
  pure $ case rs of
    [Only v] -> v
    _ -> Nothing

-- | Jobsets hydra is evaluating right now (project, jobset, started ms,
-- previous eval id).
runningEvals :: Connection -> IO [(Text, Text, Maybe Int64, Maybe Int)]
runningEvals c = query_ c
  "SELECT j.project, j.name, j.starttime::bigint * 1000, \
  \  (SELECT max(e.id) FROM jobsetevals e WHERE e.jobset_id = j.id) \
  \FROM jobsets j WHERE j.starttime IS NOT NULL ORDER BY j.starttime"
