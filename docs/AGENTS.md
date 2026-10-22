# Querying hydrometer (for AI agents and scripts)

hydrometer records what the ci.zw3rk.com build farm's machines are doing, and
which hydra build step or evaluation is responsible. There are three ways in.
All of them are read-only.

## 1. MCP (preferred)

```sh
ssh x86_64-linux-0 hydrometer mcp
```

This is a stdio Model Context Protocol server. For Claude Code, add it to
`.mcp.json`:

```json
{ "mcpServers": { "hydrometer": { "command": "ssh", "args": ["x86_64-linux-0", "hydrometer", "mcp"] } } }
```

| tool | answers |
|---|---|
| `now` | What is each machine doing right now? Slots, live CPU and memory per build, queue depth, running evaluations, alerts. |
| `top_jobs {group, metric, hours}` | What uses the most CPU, wall time, memory or network? `group` is one of package, component_kind, component, platform, job_compiler, project, jobset, job, drv_name, system, machine, run_kind. |
| `bottlenecks {hours}` | Is the farm bound by cpu, memory, network, slots or the scheduler, or is it starved? Gives minutes per class per machine. |
| `machine {name, hours}` | One machine's history: a host metrics summary and the steps it ran. |
| `recent_evals {limit}` | Recent evaluations with progress, and the ones running now. |
| `explain_eval {eval_id}` | Why is this PR slow? Time split into import, build and upload, a timeline of running steps against unfinished builds, and the longest steps. |
| `explain_build {build_id}` | The steps of a build and what each one cost. |
| `schema` | Every hydrometer view and table, with column docs. |
| `sql {query}` | Read-only SQL (30 s timeout, 500 rows). |

## 2. SQL

The data is a `hydrometer` schema inside hydra's PostgreSQL database, so it
joins directly with hydra's tables. Use `hydrometer query 'SELECT …'` on
linux-0, or the MCP `sql` tool.

**Views** (start here; each has a `COMMENT` on it):

- `hydrometer.v_step_usage`: one row per hydra build step, combining hydra's
  record with the agent's measurements.
  - From hydra: project, jobset, job, machine, started/stopped, wall_s,
    import_s, build_s, upload_s, status.
  - From the agent: cpu_s, avg_cores, peak_rss, peak_footprint, and network
    and disk bytes.
  - Classification: package, component_kind, component, platform and
    job_compiler.
  - **Filter on `stopped` (indexed) or `build`.**
- `hydrometer.v_host`: raw 5 s host samples as rates. cpu_util is 0..1;
  mem_pressure is 0..100.
- `hydrometer.v_machine_minute`: per-minute rollup, kept for a year.
- `hydrometer.v_bottleneck_minute`: what limited each machine in each minute.
- `hydrometer.v_eval_progress`: build counts per evaluation.
- `hydrometer.v_now_steps`: steps hydra says are running.

**Tables**:

- `runs`: one row per build, VM or evaluation seen by an agent. It carries
  totals and `build`/`stepnr` once matched.
- `run_samples`, `host_samples` and `class_samples`: the raw samples.
- `machine_status_samples` and `queue_samples`: the queue runner's view.

**Functions**: `hydrometer.drv_name(drvpath)` and
`hydrometer.classify_drv(drvpath)`, which returns package, kind, component,
platform and version.

### The canonical questions

What types of jobs use the most CPU?

```sql
SELECT package, component_kind, platform, count(*) AS steps,
       round(sum(cpu_s)/3600, 1) AS cpu_h, round(avg(avg_cores), 1) AS avg_cores
FROM hydrometer.v_step_usage
WHERE stopped > now() - interval '7 days' AND cpu_s IS NOT NULL
GROUP BY 1, 2, 3 ORDER BY cpu_h DESC LIMIT 20;
```

Are we memory, CPU or network bound?

```sql
SELECT bottleneck, count(*) AS machine_minutes,
       round(100.0 * count(*) / sum(count(*)) OVER (), 1) AS pct
FROM hydrometer.v_bottleneck_minute
WHERE t > now() - interval '24 hours'
GROUP BY 1 ORDER BY 2 DESC;
```

Why is this PR taking a long time? Find its latest evaluation, then the
critical facts:

```sql
SELECT e.id FROM jobsetevals e JOIN jobsets j ON j.id = e.jobset_id
WHERE j.name = 'pullrequest-2606' ORDER BY e.id DESC LIMIT 1;
-- then MCP explain_eval {eval_id}, or:
SELECT drv_name, machine, wall_s, import_s, build_s, cpu_s, avg_cores, peak_rss
FROM hydrometer.v_step_usage
WHERE build IN (SELECT build FROM jobsetevalmembers WHERE eval = $1)
ORDER BY wall_s DESC NULLS LAST LIMIT 20;
```

What is this machine doing right now? Use MCP `now`, or:

```sql
SELECT * FROM hydrometer.v_now_steps WHERE machine = 'aarch64-darwin-9';
```

A build that is hung (busy but using no CPU):

```sql
SELECT r.machine, r.name, r.build, r.stepnr, now() - r.started AS running,
       sum(rs.cpu_ms) / 1000.0 AS cpu_s_last_10m
FROM hydrometer.runs r JOIN hydrometer.run_samples rs ON rs.run_id = r.id
WHERE r.last_seen > now() - interval '1 minute' AND rs.t > now() - interval '10 minutes'
GROUP BY 1, 2, 3, 4, 5 HAVING sum(rs.cpu_ms) < 1000 ORDER BY running DESC;
```

## 3. HTTP

`GET /api` (on linux-0 at `http://127.0.0.1:9185/api`, and publicly under
`https://ci.zw3rk.com/hydrometer/api` once deployed) describes the endpoints:

- `/api/now`, `/api/top`, `/api/bottlenecks`, `/api/farm`
- `/api/machine/{name}/series`, `/api/machine/{name}/steps`
- `/api/evals`, `/api/eval/{id}`, `/api/build/{id}`
- `/api/step/{build}/{stepnr}/series`

Times are epoch milliseconds. A negative `from` means "that many ms ago".
Tables are `{columns, rows}`; series are `{time, stepMs, series}`.

## Caveats

- CPU, memory and network per step exist only for steps on machines running
  an agent, and only since the agent started. Hydra's own timings (wall,
  import, build, upload) exist for every step.
- The queue runner's `/status/machines` lists only one of each Mac's two
  builders (darwin and linux), because it is keyed by hostname. hydrometer
  takes slot counts from the agents (the `hydra-builder --max-jobs` argument)
  instead.
- `unmatched` runs are builds hydra did not schedule on that machine, mostly
  import-from-derivation during evaluation.
- The bottleneck thresholds are deliberately simple; check the numeric
  columns before drawing conclusions.
