-- hydrometer views and functions.  Re-applied on every server start (after
-- the numbered migrations), so they can change without a migration.
-- These views are the intended interface for people and AI agents: each one
-- carries a COMMENT explaining its columns.

DROP VIEW IF EXISTS hydrometer.v_now_steps, hydrometer.v_eval_progress, hydrometer.v_bottleneck_minute,
  hydrometer.v_machine_minute, hydrometer.v_step_usage, hydrometer.v_host CASCADE;
DROP FUNCTION IF EXISTS hydrometer.bottleneck_minutes(timestamptz, timestamptz) CASCADE;
DROP FUNCTION IF EXISTS hydrometer.classify_drv(text) CASCADE;
DROP FUNCTION IF EXISTS hydrometer.drv_name(text) CASCADE;
DROP TYPE IF EXISTS hydrometer.drv_class CASCADE;

-- ---------------------------------------------------------------------------
-- Derivation-name classification.

CREATE FUNCTION hydrometer.drv_name(drvpath text) RETURNS text
IMMUTABLE LANGUAGE sql AS $$
  SELECT regexp_replace(regexp_replace(drvpath, '^/nix/store/[a-z0-9]{32}-', ''), '\.drv$', '')
$$;
COMMENT ON FUNCTION hydrometer.drv_name(text) IS 'Strip the store prefix and .drv suffix: /nix/store/<hash>-foo-1.0.drv -> foo-1.0.';

CREATE TYPE hydrometer.drv_class AS (
  package   text,  -- cabal package (haskell.nix components) or derivation pname
  kind      text,  -- lib, exe, test, bench, check (running a test), setup, or other
  component text,  -- component name (haskell.nix), e.g. the sublibrary or exe name
  platform  text,  -- target triple for cross builds, 'native' otherwise
  version   text
);

CREATE FUNCTION hydrometer.classify_drv(drv text) RETURNS hydrometer.drv_class
IMMUTABLE LANGUAGE plpgsql AS $$
DECLARE
  n text := hydrometer.drv_name(drv);
  r hydrometer.drv_class;
  m text[];
  pos int;
  pkg text;
  kind text;
  rest text;
  best_pkg text;
  best_kind text;
  best_rest text;
  -- arch-vendor-os target triples as they appear in haskell.nix derivation names.
  tre text := '(x86_64|aarch64|i686|armv7a|armv6l|wasm32|javascript|riscv64)-[a-z0-9_]+-(mingw32|linux-gnu|linux-musl|linux-android|linux-androideabi|linux-gnueabihf|linux-musleabihf|wasi|ghcjs|darwin)';
BEGIN
  -- haskell.nix components are named <pkg>-<kind>-<component>[-static][-<triple>]-<version>[-check...].
  -- Package names contain dashes, so try every "-<kind>-" split and prefer
  -- the one whose component name repeats the package name (the main lib or
  -- an exe named after its package); fall back to the first split.
  pos := 0;
  LOOP
    m := regexp_match(substr(n, pos + 1), '-(lib|exe|test|bench|setup)(-|$)');
    EXIT WHEN m IS NULL;
    pos := pos + strpos(substr(n, pos + 1), '-' || m[1]);
    pkg := left(n, pos - 1);
    kind := m[1];
    rest := substr(n, pos + length(kind) + 2);
    IF best_pkg IS NULL THEN
      best_pkg := pkg; best_kind := kind; best_rest := rest;
    END IF;
    IF rest = pkg OR rest LIKE pkg || '-%' THEN
      best_pkg := pkg; best_kind := kind; best_rest := rest;
      EXIT;
    END IF;
    pos := pos + 1;
  END LOOP;

  IF best_pkg IS NULL THEN
    r.kind := 'other';
    m := regexp_match(n, '^(.*?)-([0-9][^-]*)(-.*)?$');
    IF m IS NULL THEN
      r.package := n;
    ELSE
      r.package := m[1];
      r.version := m[2];
    END IF;
    r.platform := coalesce((regexp_match(n, '(' || tre || ')'))[1], 'native');
    RETURN r;
  END IF;

  r.package := best_pkg;
  r.kind := best_kind;
  IF best_rest ~ '-check(-|$)' THEN
    r.kind := 'check';
  END IF;
  m := regexp_match(best_rest, '-([0-9][0-9.]*)(-[a-z].*)?$');
  r.version := m[1];
  r.platform := coalesce((regexp_match(best_rest, '-(' || tre || ')(-[0-9]|$)'))[1], 'native');
  r.component := regexp_replace(
    regexp_replace(best_rest, '(-(' || tre || '))?(-[0-9][0-9.]*(-.*)?)?$', ''),
    '-static$', '');
  r.component := nullif(r.component, '');
  RETURN r;
END $$;
COMMENT ON FUNCTION hydrometer.classify_drv(text) IS
  'Classify a derivation path or name. Understands haskell.nix component derivations (<pkg>-<lib|exe|test|bench>-<component>[-<triple>]-<version>); anything else is kind=other with package = name before the version.';

-- ---------------------------------------------------------------------------

CREATE VIEW hydrometer.v_host AS
SELECT
  machine, t, interval_ms,
  (cpu_user_ms + cpu_system_ms)::float8 / nullif(cpu_user_ms + cpu_system_ms + cpu_idle_ms, 0) AS cpu_util,
  (cpu_user_ms + cpu_system_ms)::float8 / nullif(interval_ms, 0) AS cores_busy,
  load1,
  mem_used, mem_total, mem_used::float8 / nullif(mem_total, 0) AS mem_frac,
  mem_compressed, mem_pressure, cpu_pressure, io_pressure,
  swap_used, pageouts,
  net_rx_bytes * 1000.0 / nullif(interval_ms, 0) AS net_rx_rate,
  net_tx_bytes * 1000.0 / nullif(interval_ms, 0) AS net_tx_rate,
  disk_read_bytes * 1000.0 / nullif(interval_ms, 0) AS disk_read_rate,
  disk_write_bytes * 1000.0 / nullif(interval_ms, 0) AS disk_write_rate,
  open_files, max_files, store_free, store_total
FROM hydrometer.host_samples;
COMMENT ON VIEW hydrometer.v_host IS
  'Raw host samples as rates: cpu_util 0..1 of all cores, cores_busy, *_rate in bytes/second, mem_pressure 0..100. Last 14 days only; use v_machine_minute for longer ranges.';

CREATE VIEW hydrometer.v_step_usage AS
SELECT
  s.build, s.stepnr,
  j.project, j.name AS jobset, b.job,
  s.system, s.machine,
  hydrometer.drv_name(s.drvpath) AS drv_name,
  (hydrometer.classify_drv(s.drvpath)).package AS package,
  (hydrometer.classify_drv(s.drvpath)).kind AS component_kind,
  (hydrometer.classify_drv(s.drvpath)).component AS component,
  (hydrometer.classify_drv(s.drvpath)).platform AS platform,
  split_part(b.job, '.', 3) AS job_compiler,
  to_timestamp(s.starttime) AS started,
  to_timestamp(s.stoptime) AS stopped,
  -- A queue-runner restart closes leftover busy steps with stopTime 0.
  CASE WHEN s.stoptime >= s.starttime THEN s.stoptime - s.starttime END AS wall_s,
  s.status, s.busy,
  (s.import_time_ms / 1000.0)::float8 AS import_s,
  (s.build_time_ms / 1000.0)::float8 AS build_s,
  (s.upload_time_ms / 1000.0)::float8 AS upload_s,
  r.kind AS run_kind,
  (r.cpu_ms / 1000.0)::float8 AS cpu_s,
  (r.cpu_ms / 1000.0 / nullif(extract(epoch FROM r.last_seen - r.first_seen), 0))::float8 AS avg_cores,
  r.peak_rss, r.peak_footprint,
  r.net_rx_bytes, r.net_tx_bytes, r.disk_read_bytes, r.disk_write_bytes,
  s.drvpath
FROM buildsteps s
JOIN builds b ON b.id = s.build
JOIN jobsets j ON j.id = b.jobset_id
LEFT JOIN hydrometer.runs r ON r.build = s.build AND r.stepnr = s.stepnr;
COMMENT ON VIEW hydrometer.v_step_usage IS
  'Every hydra build step with what it cost. From hydra: who (project/jobset/job), where (machine), when (started/stopped/wall_s), phases (import_s: copying inputs to the machine, build_s, upload_s), status (0 = success). From the agents (NULL for steps on machines without an agent or before hydrometer existed): cpu_s, avg_cores, peak_rss/peak_footprint (bytes), net/disk bytes. package/component_kind/component/platform classify the derivation; job_compiler is the 3rd dot-separated part of the job name (e.g. ghc914-sh for haskell.nix jobs). Filter on started first: buildsteps is large.';

CREATE VIEW hydrometer.v_machine_minute AS
SELECT
  machine, t,
  cpu_busy_ms::float8 / nullif(cpu_total_ms, 0) AS cpu_util,
  mem_used_max, mem_total, mem_pressure_max, swap_used_max, pageouts,
  net_rx_bytes / 60.0 AS net_rx_rate,
  net_tx_bytes / 60.0 AS net_tx_rate,
  disk_read_bytes / 60.0 AS disk_read_rate,
  disk_write_bytes / 60.0 AS disk_write_rate,
  open_files_max, store_free_min, runs_max
FROM hydrometer.host_minute;
COMMENT ON VIEW hydrometer.v_machine_minute IS 'Per-machine per-minute rollup (kept a year): cpu_util 0..1, peak memory, rates in bytes/second, runs_max = most concurrent builds seen.';

-- What limited each machine in each minute of [from_t, to_t).  A function so that
-- the range reaches every step: as a plain view, the per-minute run and
-- queue aggregates covered all samples ever kept whatever range the caller
-- asked for, and the Bottlenecks page's queries ran for 20 minutes.
CREATE FUNCTION hydrometer.bottleneck_minutes(from_t timestamptz, to_t timestamptz)
RETURNS TABLE (machine text, t timestamptz, cpu_util float8, mem_pressure_max real,
               pageouts bigint, net_rx_rate numeric, builds bigint, slots bigint,
               runnable bigint, bottleneck text)
LANGUAGE sql STABLE AS $$
WITH builder AS (
  SELECT m.name AS machine, b.ord AS builder, (b.value->>'maxJobs')::int AS max_jobs,
         ARRAY(SELECT jsonb_array_elements_text(b.value->'systems')) AS systems
  FROM hydrometer.machines m,
       jsonb_array_elements(coalesce(m.builders, '[]'::jsonb)) WITH ORDINALITY AS b(value, ord)
), mm AS (
  SELECT * FROM hydrometer.v_machine_minute m WHERE m.t >= from_t AND m.t < to_t
), runs_min AS MATERIALIZED (
  -- Most builds running at once per machine, minute and system.
  SELECT machine, t, system, max(n) AS n FROM (
    SELECT r.machine, date_trunc('minute', rs.t) AS t, rs.t AS st, r.system, count(*) AS n
    FROM hydrometer.run_samples rs JOIN hydrometer.runs r ON r.id = rs.run_id
    WHERE r.kind <> 'eval' AND rs.t >= from_t AND rs.t < to_t
    GROUP BY 1, 2, 3, 4) x
  GROUP BY 1, 2, 3
), queue AS MATERIALIZED (
  SELECT date_trunc('minute', q.t) AS t, q.system, max(q.runnable) AS runnable
  FROM hydrometer.queue_samples q WHERE q.t >= from_t AND q.t < to_t
  GROUP BY 1, 2
), per_builder AS (
  SELECT m.machine, m.t, b.builder, b.max_jobs, b.systems, coalesce(sum(rm.n), 0) AS busy
  FROM mm m JOIN builder b ON b.machine = m.machine
  LEFT JOIN runs_min rm ON rm.machine = m.machine AND rm.t = m.t AND rm.system = ANY (b.systems)
  GROUP BY 1, 2, 3, 4, 5
), per_builder_q AS (
  SELECT p.machine, p.t, p.builder, p.max_jobs, p.busy, coalesce(sum(q.runnable), 0) AS runnable
  FROM per_builder p
  LEFT JOIN LATERAL unnest(p.systems) AS s(system) ON true
  LEFT JOIN queue q ON q.t = p.t AND q.system = s.system
  GROUP BY 1, 2, 3, 4, 5
), agg AS (
  SELECT machine, t, sum(busy) AS builds, sum(max_jobs) AS slots, sum(runnable) AS runnable,
    bool_or(busy >= max_jobs AND runnable > 0) AS full_with_work,
    bool_or(busy < max_jobs AND runnable > 0) AS free_with_work
  FROM per_builder_q GROUP BY 1, 2
)
SELECT
  m.machine, m.t, m.cpu_util, m.mem_pressure_max, m.pageouts, m.net_rx_rate,
  coalesce(a.builds, m.runs_max)::bigint AS builds, a.slots::bigint, a.runnable::bigint,
  CASE
    WHEN m.cpu_util >= 0.85 THEN 'cpu'
    WHEN m.mem_pressure_max >= 50 OR m.pageouts > 2000 THEN 'memory'
    WHEN m.net_rx_rate >= 80e6 THEN 'network'
    WHEN a.full_with_work THEN 'slots'
    WHEN a.free_with_work THEN 'scheduler'
    WHEN coalesce(a.builds, m.runs_max, 0) = 0 THEN 'idle'
    ELSE 'starved'
  END AS bottleneck
FROM mm m
LEFT JOIN agg a ON a.machine = m.machine AND a.t = m.t
$$;
COMMENT ON FUNCTION hydrometer.bottleneck_minutes(timestamptz, timestamptz) IS
  'v_bottleneck_minute for the minutes in [from_t, to_t).  Use this rather than filtering the view: the view covers all time.';

CREATE VIEW hydrometer.v_bottleneck_minute AS
SELECT * FROM hydrometer.bottleneck_minutes('-infinity', 'infinity');
COMMENT ON VIEW hydrometer.v_bottleneck_minute IS
  'What limited each machine in each minute, over all time: for a range use hydrometer.bottleneck_minutes(from, to), which is far faster. bottleneck: cpu (>=85% of cores busy), memory (pressure >= 50 or heavy paging), network (>= 80 MB/s in), slots (a builder on the machine has every slot busy while steps for its systems are runnable, without saturating the hardware: more slots or bigger VMs would help), scheduler (a builder has free slots while steps for its systems are runnable: the queue runner is not dispatching), starved (nothing runnable for it: waiting on dependencies built elsewhere), idle. Slots come from the agents (hydrometer.machines.builders, one entry per hydra builder: a Mac has a darwin and a linux one); builds counts concurrent agent runs. Thresholds are deliberately simple; check the numeric columns.';

CREATE VIEW hydrometer.v_eval_progress AS
SELECT
  e.id AS eval_id, j.project, j.name AS jobset, to_timestamp(e.timestamp) AS evaluated,
  e.evaltime AS eval_s,
  count(b.id) AS builds,
  count(*) FILTER (WHERE b.finished = 1) AS finished,
  count(*) FILTER (WHERE b.finished = 1 AND b.buildstatus = 0) AS succeeded,
  count(*) FILTER (WHERE b.finished = 1 AND b.buildstatus <> 0) AS failed,
  count(*) FILTER (WHERE b.finished = 0) AS queued,
  to_timestamp(max(b.stoptime)) AS last_finish
FROM jobsetevals e
JOIN jobsets j ON j.id = e.jobset_id
LEFT JOIN jobsetevalmembers em ON em.eval = e.id
LEFT JOIN builds b ON b.id = em.build
GROUP BY e.id, j.project, j.name, e.timestamp, e.evaltime;
COMMENT ON VIEW hydrometer.v_eval_progress IS 'Build counts per hydra evaluation. Filter on eval_id or evaluated: this aggregates over all evaluations.';

CREATE VIEW hydrometer.v_now_steps AS
SELECT
  s.machine, s.build, s.stepnr, j.project, j.name AS jobset, b.job,
  hydrometer.drv_name(s.drvpath) AS drv_name, s.drvpath, s.system,
  to_timestamp(s.starttime) AS started, s.busy,
  r.kind AS run_kind, r.cpu_ms, r.peak_rss, r.last_seen
FROM buildsteps s
JOIN builds b ON b.id = s.build
JOIN jobsets j ON j.id = b.jobset_id
LEFT JOIN hydrometer.runs r ON r.build = s.build AND r.stepnr = s.stepnr
WHERE s.busy <> 0 AND b.finished = 0;
COMMENT ON VIEW hydrometer.v_now_steps IS 'Build steps hydra says are in progress now, with the agent''s run if matched.';
