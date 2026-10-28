-- macOS vnodes.  Every open file needs one; when none is free, opening a file
-- fails with ENFILE ("Too many open files in system") however high
-- kern.maxfiles is.  Linux VMs' VirtioFS shares hold one per inode the guest
-- has cached, so long VM builds use them up (ican-group/haskell.nix evals 3091
-- and 3093 lost dozens of GHC steps on the M4s this way).
ALTER TABLE hydrometer.host_samples
  ADD COLUMN free_vnodes bigint,
  ADD COLUMN max_vnodes bigint;
ALTER TABLE hydrometer.host_minute
  ADD COLUMN free_vnodes_min bigint,
  ADD COLUMN max_vnodes bigint;
