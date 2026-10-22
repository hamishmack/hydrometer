-- hydrometer schema, version 1.
--
-- Lives in its own schema inside hydra's database so that every metric can
-- be joined directly against hydra's builds / buildsteps / jobsets tables.
-- Large sample tables are partitioned by day so retention is a cheap DROP of
-- old partitions (see hydrometer.ensure_partitions / drop_partitions_before).

CREATE SCHEMA IF NOT EXISTS hydrometer;
COMMENT ON SCHEMA hydrometer IS
  'hydrometer: resource usage of the build farm, attributed to hydra build steps and evaluations. Start with the v_* views; see docs/AGENTS.md.';

CREATE TABLE hydrometer.machines (
  name          text PRIMARY KEY,
  os            text,
  os_version    text,
  cpu_count     int,
  mem_total     bigint,
  agent_url     text,
  agent_version text,
  agent_start   timestamptz,
  last_seq      bigint,
  last_sample   timestamptz,
  boot_time     timestamptz
);
COMMENT ON TABLE hydrometer.machines IS 'One row per machine with an agent. name matches hydra''s buildsteps.machine.';

CREATE TABLE hydrometer.host_samples (
  machine          text        NOT NULL,
  t                timestamptz NOT NULL,
  interval_ms      int         NOT NULL,
  cpu_user_ms      bigint,
  cpu_system_ms    bigint,
  cpu_idle_ms      bigint,
  load1            real,
  load5            real,
  load15           real,
  mem_total        bigint,
  mem_used         bigint,
  mem_compressed   bigint,
  mem_pressure     real,
  cpu_pressure     real,
  io_pressure      real,
  swap_used        bigint,
  swap_total       bigint,
  pageouts         bigint,
  net_rx_bytes     bigint,
  net_tx_bytes     bigint,
  disk_read_bytes  bigint,
  disk_write_bytes bigint,
  open_files       bigint,
  max_files        bigint,
  store_free       bigint,
  store_total      bigint,
  PRIMARY KEY (machine, t)
) PARTITION BY RANGE (t);
COMMENT ON TABLE hydrometer.host_samples IS
  'Raw per-host samples (every 5 s). *_ms and *_bytes and pageouts are totals over the interval ending at t; the rest are levels at t. CPU ms are summed over all cores. mem_pressure is 0 (none) to 100 (critical).';

CREATE TABLE hydrometer.runs (
  id               bigserial PRIMARY KEY,
  machine          text        NOT NULL,
  run_key          text        NOT NULL,
  kind             text        NOT NULL,
  name             text        NOT NULL,
  outputs          text[]      NOT NULL DEFAULT '{}',
  system           text,
  started          timestamptz NOT NULL,
  first_seen       timestamptz NOT NULL,
  last_seen        timestamptz NOT NULL,
  cpu_ms           bigint      NOT NULL DEFAULT 0,
  peak_rss         bigint      NOT NULL DEFAULT 0,
  peak_footprint   bigint      NOT NULL DEFAULT 0,
  disk_read_bytes  bigint      NOT NULL DEFAULT 0,
  disk_write_bytes bigint      NOT NULL DEFAULT 0,
  net_rx_bytes     bigint      NOT NULL DEFAULT 0,
  net_tx_bytes     bigint      NOT NULL DEFAULT 0,
  max_procs        int         NOT NULL DEFAULT 0,
  samples          int         NOT NULL DEFAULT 0,
  drvpath          text,
  build            int,
  stepnr           int,
  jobset_id        int,
  eval_id          int,
  resolution       text,
  resolved_at      timestamptz,
  UNIQUE (machine, run_key)
);
CREATE INDEX runs_build_step ON hydrometer.runs (build, stepnr);
CREATE INDEX runs_last_seen ON hydrometer.runs (last_seen);
CREATE INDEX runs_unresolved ON hydrometer.runs (last_seen) WHERE resolution IS NULL;
CREATE INDEX runs_drvpath ON hydrometer.runs (drvpath);
COMMENT ON TABLE hydrometer.runs IS
  'One row per unit of attributable work seen by an agent: kind = vm (linux build in a nix-linux-builder VM), native (local nix build) or eval (hydra-eval-jobset). Totals are kept up to date as samples arrive. build/stepnr link to hydra.buildsteps; resolution is step (matched), ifd (a build hydra did not schedule, e.g. import-from-derivation during evaluation), eval, or unmatched.';
COMMENT ON COLUMN hydrometer.runs.cpu_ms IS 'CPU time used by the run (all processes, all cores), milliseconds.';
COMMENT ON COLUMN hydrometer.runs.peak_footprint IS 'Peak physical memory footprint (darwin phys_footprint; RSS on linux).';

CREATE TABLE hydrometer.run_samples (
  run_id           bigint      NOT NULL,
  t                timestamptz NOT NULL,
  cpu_ms           int,
  rss              bigint,
  footprint        bigint,
  disk_read_bytes  bigint,
  disk_write_bytes bigint,
  net_rx_bytes     bigint,
  net_tx_bytes     bigint,
  procs            int,
  PRIMARY KEY (run_id, t)
) PARTITION BY RANGE (t);
COMMENT ON TABLE hydrometer.run_samples IS 'Per-run samples (every 5 s). cpu_ms and *_bytes are totals over the sample interval.';

CREATE TABLE hydrometer.class_samples (
  machine          text        NOT NULL,
  t                timestamptz NOT NULL,
  class            text        NOT NULL,
  cpu_ms           int,
  rss              bigint,
  disk_read_bytes  bigint,
  disk_write_bytes bigint,
  net_rx_bytes     bigint,
  net_tx_bytes     bigint,
  procs            int,
  PRIMARY KEY (machine, t, class)
) PARTITION BY RANGE (t);
COMMENT ON TABLE hydrometer.class_samples IS
  'Usage by processes outside any run, grouped by role: nix-daemon (substitution, store I/O), hydra-builder (input import and output upload), peernix, vm-unattributed, hydrometer, system, other.';

CREATE TABLE hydrometer.machine_status_samples (
  t                     timestamptz NOT NULL,
  machine               text        NOT NULL,
  systems               text[],
  max_jobs              int,
  current_jobs          int,
  disabled              boolean,
  consecutive_failures  int,
  load1                 real,
  mem_usage             bigint,
  nr_steps_done         bigint,
  avg_step_import_ms    bigint,
  avg_step_build_ms     bigint,
  uploading_paths       int,
  downloading_paths     int,
  PRIMARY KEY (machine, t)
);
CREATE INDEX machine_status_samples_t ON hydrometer.machine_status_samples (t);
COMMENT ON TABLE hydrometer.machine_status_samples IS
  'What the hydra queue runner believes about each machine (polled every 15 s from its /status/machines). machine names here are the queue runner''s.';

CREATE TABLE hydrometer.queue_samples (
  t                   timestamptz NOT NULL,
  system              text        NOT NULL,
  runnable            int         NOT NULL,
  oldest_runnable_ms  bigint,
  PRIMARY KEY (t, system)
);
COMMENT ON TABLE hydrometer.queue_samples IS
  'Steps whose dependencies are all built and that are waiting for a machine, per system (from the queue runner, every 15 s). Persistent runnable > 0 while machines have free slots means the scheduler is the bottleneck.';

CREATE TABLE hydrometer.host_minute (
  machine          text        NOT NULL,
  t                timestamptz NOT NULL,
  samples          int,
  cpu_busy_ms      bigint,
  cpu_total_ms     bigint,
  mem_used_max     bigint,
  mem_total        bigint,
  mem_pressure_max real,
  swap_used_max    bigint,
  pageouts         bigint,
  net_rx_bytes     bigint,
  net_tx_bytes     bigint,
  disk_read_bytes  bigint,
  disk_write_bytes bigint,
  open_files_max   bigint,
  store_free_min   bigint,
  runs_max         int,
  PRIMARY KEY (machine, t)
);
COMMENT ON TABLE hydrometer.host_minute IS 'Per-minute rollup of host_samples, kept for a year.';

-- ---------------------------------------------------------------------------
-- Partition maintenance.

CREATE FUNCTION hydrometer.ensure_partitions(days_ahead int DEFAULT 2) RETURNS void
LANGUAGE plpgsql AS $$
DECLARE
  tbl text;
  d date;
BEGIN
  FOREACH tbl IN ARRAY ARRAY['host_samples', 'run_samples', 'class_samples'] LOOP
    FOR i IN -1 .. days_ahead LOOP
      d := current_date + i;
      EXECUTE format(
        'CREATE TABLE IF NOT EXISTS hydrometer.%I PARTITION OF hydrometer.%I FOR VALUES FROM (%L) TO (%L)',
        tbl || '_' || to_char(d, 'YYYYMMDD'), tbl, d::timestamptz, (d + 1)::timestamptz);
    END LOOP;
  END LOOP;
END $$;

CREATE FUNCTION hydrometer.drop_partitions_before(cutoff date) RETURNS int
LANGUAGE plpgsql AS $$
DECLARE
  r record;
  n int := 0;
BEGIN
  FOR r IN
    SELECT c.relname
    FROM pg_inherits i
    JOIN pg_class c ON c.oid = i.inhrelid
    JOIN pg_class p ON p.oid = i.inhparent
    JOIN pg_namespace ns ON ns.oid = p.relnamespace
    WHERE ns.nspname = 'hydrometer'
      AND p.relname IN ('host_samples', 'run_samples', 'class_samples')
      AND to_date(right(c.relname, 8), 'YYYYMMDD') < cutoff
  LOOP
    EXECUTE format('DROP TABLE hydrometer.%I', r.relname);
    n := n + 1;
  END LOOP;
  RETURN n;
END $$;

SELECT hydrometer.ensure_partitions();
