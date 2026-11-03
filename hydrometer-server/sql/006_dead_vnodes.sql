-- macOS dead vnodes.  macOS never shrinks its vnode table, so it fills up
-- and kern.free_vnodes stays low, but most entries are dead vnodes that the
-- kernel reuses first and kern.free_vnodes does not count (2026-11-03: about
-- 235,000 of 248,000 on each M4).  What is available is free plus dead.
ALTER TABLE hydrometer.host_samples
  ADD COLUMN dead_vnodes bigint;
ALTER TABLE hydrometer.host_minute
  ADD COLUMN available_vnodes_min bigint;
