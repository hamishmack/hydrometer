-- Build capacity per machine, as the agent reads it from the hydra builders'
-- command lines: [{"systems": [...], "maxJobs": n}, ...].  The queue
-- runner's own status keys machines by hostname and so shows only one of a
-- Mac's two builders (darwin and linux).
ALTER TABLE hydrometer.machines ADD COLUMN builders jsonb;
COMMENT ON COLUMN hydrometer.machines.builders IS 'Hydra builders on this machine: [{systems, maxJobs}], from the agent.';
