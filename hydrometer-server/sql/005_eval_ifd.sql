-- Evaluation-time builds (import-from-derivation) and nix-eval-jobs workers,
-- from the trace lines in hydra-evaluator's journal (see
-- Hydrometer.Server.EvalIfd).  Each worker blocks on an IFD until it is
-- built, so one slow build can hold every worker of an evaluation
-- (2026-11-03: eight workers idle for over an hour behind one wasm LLVM).
--
-- IFD and build events are kept one row per event or per (drv, status);
-- nix-eval-jobs' per-job lines (~10k per evaluation) are folded into one row
-- per worker, since only their totals are ever shown.

CREATE TABLE hydrometer.evaluations (
  id              bigserial PRIMARY KEY,
  pid             int         NOT NULL,
  project         text        NOT NULL,
  jobset          text        NOT NULL,
  first_seen      timestamptz NOT NULL,
  last_seen       timestamptz NOT NULL,
  jobset_id       int,
  eval_id         int,
  all_waiting_ms  bigint      NOT NULL DEFAULT 0
);
CREATE INDEX evaluations_jobset ON hydrometer.evaluations (project, jobset, first_seen);
CREATE INDEX evaluations_eval ON hydrometer.evaluations (eval_id);
CREATE INDEX evaluations_last_seen ON hydrometer.evaluations (last_seen);
COMMENT ON TABLE hydrometer.evaluations IS 'One row per hydra-eval-jobset process (pid) that logged IFD or nix-eval-jobs trace lines in hydra-evaluator''s journal. eval_id is hydra''s jobsetevals.id once hydra has written it (NULL for an evaluation that is running, failed, or found nothing new). all_waiting_ms: approximately how long every live worker was blocked on an IFD at once.';

CREATE TABLE hydrometer.eval_workers (
  evaluation   bigint      NOT NULL REFERENCES hydrometer.evaluations ON DELETE CASCADE,
  pid          int         NOT NULL,
  spawned      timestamptz,
  init_ms      int,
  first_seen   timestamptz NOT NULL,
  last_seen    timestamptz NOT NULL,
  exited       timestamptz,
  exit_reason  text,
  life_ms      bigint,
  jobs         int         NOT NULL DEFAULT 0,
  job_errors   int         NOT NULL DEFAULT 0,
  eval_ms      bigint      NOT NULL DEFAULT 0,
  ifds         int         NOT NULL DEFAULT 0,
  ifd_ms       bigint      NOT NULL DEFAULT 0,
  rss_mb       int,
  max_rss_mb   int,
  last_attr    text,
  PRIMARY KEY (evaluation, pid)
);
COMMENT ON TABLE hydrometer.eval_workers IS 'One row per nix-eval-jobs worker process of an evaluation, totalled from its nej-trace lines: spawned (worker-ready minus init_ms), exited/exit_reason (max-memory: restarted to free memory; exit: done; NULL: running, or crashed), jobs (attributes evaluated; job_errors of them failed), eval_ms (time in jobs), ifds/ifd_ms (IFDs and time blocked on them, within jobs), last_seen (its last line).';

CREATE TABLE hydrometer.eval_ifd_waits (
  evaluation  bigint      NOT NULL REFERENCES hydrometer.evaluations ON DELETE CASCADE,
  pid         int         NOT NULL,
  since       timestamptz NOT NULL,
  drvs        text[]      NOT NULL,
  at          text,
  PRIMARY KEY (evaluation, pid)
);
COMMENT ON TABLE hydrometer.eval_ifd_waits IS 'IFDs workers are blocked on now: an "IFD start" line from worker pid with no "IFD #n" line after it. drvs are .drv paths (outputs stripped); at is the source position, when nix knows it.';

CREATE TABLE hydrometer.eval_ifds (
  evaluation  bigint      NOT NULL REFERENCES hydrometer.evaluations ON DELETE CASCADE,
  drv         text        NOT NULL,
  status      text        NOT NULL,
  n           int         NOT NULL,
  pids        int[]       NOT NULL DEFAULT '{}',
  wait_ms     bigint      NOT NULL,
  max_ms      int         NOT NULL,
  first_done  timestamptz NOT NULL,
  last_done   timestamptz NOT NULL,
  PRIMARY KEY (evaluation, drv, status)
);
COMMENT ON TABLE hydrometer.eval_ifds IS 'Finished IFDs per evaluation, derivation and status ("IFD #n" lines): n of them, by the workers in pids, blocking them wait_ms in total. status is built, substituted, already-valid (a worker that waited on another''s build reports this, with a long wait), resolves-to-already-valid, permanent-failure, transient-failure, cached-failure, timed-out, input-rejected, output-rejected, ...; n > 1 with a failure status means the same failing build was retried.';

CREATE TABLE hydrometer.eval_builds (
  evaluation  bigint      NOT NULL REFERENCES hydrometer.evaluations ON DELETE CASCADE,
  t           timestamptz NOT NULL,
  drv         text        NOT NULL,
  host        text,
  failed      boolean     NOT NULL,
  error       text
);
CREATE INDEX eval_builds_eval ON hydrometer.eval_builds (evaluation, t);
COMMENT ON TABLE hydrometer.eval_builds IS 'Builds an evaluation started, for an IFD or one of its dependencies ("building ''…'' on ''…''", failed = false), and the ones that failed ("error: build of ''…'' failed", failed = true). host is the remote builder (hydra''s machine name), NULL for a local build.';

-- Where each journal scanner got to, so a restart neither skips nor
-- double-counts lines.
CREATE TABLE hydrometer.journal_cursors (
  scanner  text PRIMARY KEY,
  cursor   text NOT NULL,
  t        timestamptz NOT NULL DEFAULT now()
);
