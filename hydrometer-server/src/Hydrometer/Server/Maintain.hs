-- | Background maintenance: attributing runs to hydra steps and
-- evaluations, minute rollups, partitions and retention.
module Hydrometer.Server.Maintain
  ( runMaintenance
  , resolveRuns
  , rollup
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when)
import Data.Int (Int64)
import Data.IORef
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import System.IO (hPutStrLn, stderr)

import Hydrometer.Server.Db

-- | Match runs to hydra build steps by output path.  The same output can
-- belong to several step rows (retries, or a step re-run in another build);
-- prefer the one on the same machine that started closest to the run.
resolveRuns :: Connection -> IO Int64
resolveRuns c = do
  steps <- execute_ c
    "WITH cand AS ( \
    \  SELECT r.id, s.build, s.stepnr, s.drvpath, \
    \         row_number() OVER (PARTITION BY r.id ORDER BY (s.machine = r.machine) DESC, \
    \           abs(coalesce(s.starttime, 0) - extract(epoch FROM r.started))) AS rn \
    \  FROM hydrometer.runs r \
    \  JOIN buildstepoutputs o ON o.path = ANY (r.outputs) \
    \  JOIN buildsteps s ON s.build = o.build AND s.stepnr = o.stepnr \
    \  WHERE r.resolution IS NULL AND r.kind <> 'eval' AND r.outputs <> '{}' \
    \) \
    \UPDATE hydrometer.runs r SET build = cand.build, stepnr = cand.stepnr, drvpath = cand.drvpath, \
    \  resolution = 'step', resolved_at = now() \
    \FROM cand WHERE cand.id = r.id AND cand.rn = 1"
  -- hydra writes an evaluation's row when it finishes; match the first one
  -- of that jobset to start after the run did.
  evals <- execute_ c
    "UPDATE hydrometer.runs r SET jobset_id = j.id, eval_id = ( \
    \    SELECT e.id FROM jobsetevals e WHERE e.jobset_id = j.id \
    \      AND e.timestamp >= extract(epoch FROM r.started) - 60 ORDER BY e.id LIMIT 1), \
    \  resolution = 'eval', resolved_at = now() \
    \FROM jobsets j \
    \WHERE r.resolution IS NULL AND r.kind = 'eval' AND j.project || ':' || j.name = r.name \
    \  AND EXISTS (SELECT 1 FROM jobsetevals e WHERE e.jobset_id = j.id \
    \      AND e.timestamp >= extract(epoch FROM r.started) - 60)"
  -- Runs that ended a while ago without a step are builds hydra did not
  -- schedule: import-from-derivation during evaluation, or a manual build.
  unmatched <- execute_ c
    "UPDATE hydrometer.runs SET resolution = 'unmatched', resolved_at = now() \
    \WHERE resolution IS NULL AND last_seen < now() - interval '15 minutes'"
  pure (steps + evals + unmatched)

-- | Roll the last few minutes of host samples up into host_minute.  Re-run
-- over a window so late samples are folded in.
rollup :: Connection -> IO ()
rollup c = void $ execute_ c
  "WITH runs_at AS ( \
  \  SELECT r.machine, rs.t, count(*) AS n FROM hydrometer.run_samples rs \
  \  JOIN hydrometer.runs r ON r.id = rs.run_id \
  \  WHERE rs.t >= now() - interval '20 minutes' AND r.kind <> 'eval' GROUP BY 1, 2 \
  \), mins AS ( \
  \  SELECT h.machine, date_trunc('minute', h.t) AS t, count(*) AS samples, \
  \    sum(h.cpu_user_ms + h.cpu_system_ms) AS busy, sum(h.cpu_user_ms + h.cpu_system_ms + h.cpu_idle_ms) AS total, \
  \    max(h.mem_used) AS mem_used, max(h.mem_total) AS mem_total, max(h.mem_pressure) AS pressure, \
  \    max(h.swap_used) AS swap, sum(h.pageouts) AS pageouts, sum(h.net_rx_bytes) AS rx, sum(h.net_tx_bytes) AS tx, \
  \    sum(h.disk_read_bytes) AS dr, sum(h.disk_write_bytes) AS dw, max(h.open_files) AS files, min(h.store_free) AS store, \
  \    coalesce(max(ra.n), 0) AS runs \
  \  FROM hydrometer.host_samples h \
  \  LEFT JOIN runs_at ra ON ra.machine = h.machine AND ra.t = h.t \
  \  WHERE h.t >= now() - interval '20 minutes' \
  \  GROUP BY 1, 2 \
  \) \
  \INSERT INTO hydrometer.host_minute \
  \SELECT machine, t, samples, busy, total, mem_used, mem_total, pressure, swap, pageouts, rx, tx, dr, dw, files, store, runs FROM mins \
  \ON CONFLICT (machine, t) DO UPDATE SET samples = excluded.samples, cpu_busy_ms = excluded.cpu_busy_ms, \
  \  cpu_total_ms = excluded.cpu_total_ms, mem_used_max = excluded.mem_used_max, mem_total = excluded.mem_total, \
  \  mem_pressure_max = excluded.mem_pressure_max, swap_used_max = excluded.swap_used_max, pageouts = excluded.pageouts, \
  \  net_rx_bytes = excluded.net_rx_bytes, net_tx_bytes = excluded.net_tx_bytes, \
  \  disk_read_bytes = excluded.disk_read_bytes, disk_write_bytes = excluded.disk_write_bytes, \
  \  open_files_max = excluded.open_files_max, store_free_min = excluded.store_free_min, runs_max = excluded.runs_max"

retention :: Connection -> Int -> IO ()
retention c days = do
  _ <- query c "SELECT hydrometer.drop_partitions_before((current_date - ?::int))" (Only days) :: IO [Only Int]
  void $ execute c "DELETE FROM hydrometer.machine_status_samples WHERE t < now() - make_interval(days => ?)" (Only days)
  void $ execute c "DELETE FROM hydrometer.queue_samples WHERE t < now() - make_interval(days => ?)" (Only days)
  void $ execute c "DELETE FROM hydrometer.log_counts WHERE t < now() - make_interval(days => ?)" (Only days)
  void $ execute_ c "DELETE FROM hydrometer.cache_samples WHERE t < now() - interval '400 days'"
  void $ execute_ c "DELETE FROM hydrometer.host_minute WHERE t < now() - interval '400 days'"
  void $ execute_ c "DELETE FROM hydrometer.runs WHERE last_seen < now() - interval '400 days'"

runMaintenance :: Db -> Int -> IO ()
runMaintenance d days = do
  lastHourly <- newIORef (Nothing :: Maybe UTCTime)
  forever $ do
    now <- getCurrentTime
    hourly <- readIORef lastHourly
    let dueHourly = maybe True (\t -> diffUTCTime now t > 3600) hourly
    step "partitions" $ when dueHourly $ withConn d $ \c ->
      void (query_ c "SELECT hydrometer.ensure_partitions()" :: IO [Only ()])
    step "resolve" $ void $ withConn d resolveRuns
    step "rollup" $ withConn d rollup
    step "retention" $ when dueHourly $ withConn d (`retention` days)
    when dueHourly $ writeIORef lastHourly (Just now)
    threadDelay 30000000
  where
    step :: String -> IO () -> IO ()
    step what act = do
      r <- try act :: IO (Either SomeException ())
      either (\e -> hPutStrLn stderr ("maintenance " <> what <> ": " <> show e)) pure r
