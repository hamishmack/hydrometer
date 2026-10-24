-- Whether finished outputs reach the binary cache, sampled every 10 minutes.
CREATE TABLE hydrometer.cache_samples (
  t           timestamptz PRIMARY KEY,
  sampled     int NOT NULL,
  present     int NOT NULL,
  -- hydra-attic-bridge's queue (DrvpathsToUpload), when readable.
  queued      int,
  retrying    int,
  stale_new   int
);
COMMENT ON TABLE hydrometer.cache_samples IS 'Every 10 minutes: of `sampled` outputs of steps that finished 30-120 minutes earlier, how many had a narinfo on the cache (`present`); and hydra-attic-bridge''s queue: paths queued, how many have failed at least once (`retrying`), and how many were never tried after an hour (`stale_new`).';

-- Errors and warnings in hydra's journal, grouped by message pattern.
CREATE TABLE hydrometer.log_counts (
  t        timestamptz NOT NULL,
  unit     text NOT NULL,
  level    text NOT NULL,
  pattern  text NOT NULL,
  n        int NOT NULL,
  example  text NOT NULL,
  PRIMARY KEY (t, unit, level, pattern)
);
CREATE INDEX ON hydrometer.log_counts (t);
COMMENT ON TABLE hydrometer.log_counts IS 'Per minute: error and warning lines in the journal of each scanned systemd unit, grouped by `pattern` (the message with store paths, hashes, ids and numbers replaced), with one `example` line.';
