# hydrometer

Resource monitoring for a [Hydra](https://github.com/NixOS/hydra) build farm,
with every CPU-second, byte of memory and byte on the network attributed to
the build step or evaluation that used it.

It exists to answer questions like:

- *What is this machine doing right now?*
- *What kinds of jobs use the most CPU (or memory, or network)?*
- *Is the farm CPU-bound, memory-bound, network-bound, or waiting on the scheduler?*
- *Why is this PR taking so long to build?*

and to let both people (a dashboard) and AI agents (SQL views, a JSON API and
an MCP server) ask them.

> **Built with AI.** hydrometer was designed and written by Claude (Anthropic's
> AI model, via Claude Code), working with and reviewed by Hamish Mackenzie.
> Treat it as you would any young codebase: read the code before trusting it
> with anything important.

## How it works

```
 builder (macOS / Linux)              hydra server (linux-0)
 hydrometer-agent  ── :9184 ─────▶  hydrometer serve ──▶ postgres: hydra DB, schema `hydrometer`
   every 5 s: host + per-build         ├ pulls each agent's ring buffer (GET /samples?since=N)
   samples in a 1 h ring buffer        ├ polls the queue runner's /status/machines, /status/queues
                                       ├ matches runs to hydra build steps by output path
                                       ├ minute rollups, daily partitions, retention
                                       ├ JSON API + dashboard on :9185
                                       └ `hydrometer mcp` (stdio) for AI agents
```

**Agent** (`hydrometer-agent`). Runs as root on each builder and samples:

- **Host**: CPU, memory and memory pressure, swap and paging, network, disk,
  open files against `kern.maxfiles`, and nix store free space.
- **Runs**: units of attributable work.
  - A linux build on a Mac runs in its own `nix-linux-builder` VM. The VM's
    XPC process runs as the same `_nixbldN` user as its builder, so the two
    are joined. `build.json` names the derivation's outputs.
  - A native build is the process tree nix-daemon starts as a build user. Its
    outputs come from the builder's environment (`KERN_PROCARGS2` on macOS,
    `/proc/<pid>/environ` on Linux).
  - A hydra evaluation is `hydra-eval-jobset` and its workers.

  A run's CPU includes the reaped children of its processes, so short-lived
  compiler invocations are not lost between samples. On macOS, per-process
  network bytes come from `nettop`; there is no public API for that.
- **Everything else**, by role: nix-daemon (substitution), hydra-builder
  (importing inputs and uploading outputs), peernix and so on.

The agent never connects out. It only serves `GET /samples?since=N`, `/now`
and `/`, so a server outage loses nothing that is still in the buffer.

**Server** (`hydrometer serve`). Writes everything to a `hydrometer` schema
*inside hydra's own database*, so metrics join directly against `builds`,
`buildsteps` and `jobsets`.

- Runs are matched to build steps through `buildstepoutputs.path`.
- Runs that hydra did not schedule are marked `unmatched`. These are usually
  import-from-derivation builds during an evaluation.
- Raw samples are kept for 14 days in daily partitions. Minute rollups and
  per-run totals are kept for a year.

**Dashboard** (`hydrometer-ui`). A reflex-dom app compiled with GHC's
JavaScript backend, with charts by [Apache ECharts](https://echarts.apache.org).
Its pages are:

- **Now**: every machine, its slots and live load, queue depth, running
  evaluations, and alerts. One alert is a stalled-queue-runner detector: no
  step has finished in 15 minutes while work is runnable and slots are free.
- **Machine**: host metrics over time and a Gantt chart of its steps.
- **Jobs**: the heaviest packages, components, platforms, compilers, jobsets
  and jobs.
- **Bottlenecks**: machine-minutes classified as cpu, memory, network, slots,
  scheduler, starved or idle.
- **Evaluations** / **Evaluation**: how a PR's time is split between
  importing, building and uploading, a timeline of running steps against
  unfinished builds, and the longest steps.

## For AI agents

See [docs/AGENTS.md](docs/AGENTS.md). In short:

- **MCP**: `ssh x86_64-linux-0 hydrometer mcp` is a stdio MCP server. Its
  tools are `now`, `top_jobs`, `bottlenecks`, `machine`, `recent_evals`,
  `explain_eval`, `explain_build`, `schema` and `sql` (read-only).
- **SQL**: the `hydrometer.v_*` views carry `COMMENT`s describing every
  column.
- **HTTP**: `GET /api` describes the JSON API.

## Building

Everything is built with [haskell.nix](https://github.com/input-output-hk/haskell.nix)
(`ghc914-sh`, the v2 slice builder):

```sh
nix build .#hydrometer-agent     # macOS or Linux
nix build .#hydrometer           # the server (Linux)
nix build .#hydrometer-site      # the dashboard (GHC JavaScript backend)
```

For development, `nix develop` gives a shell with the toolchain, and
`cabal build all` builds natively. The native `hydrometer-ui` is a
jsaddle-warp development server: run it with
`HYDROMETER_API=http://127.0.0.1:9185` and open http://localhost:3912/.

## Deploying

`nixosModules.default` provides `services.hydrometer` (the server, an
optional agent for the host, database roles, and an nginx location).
`darwinModules.agent` provides `services.hydrometer-agent` (a launchd
daemon).

```nix
services.hydrometer = {
  enable = true;
  agents = map (n: { name = "aarch64-darwin-${n}"; url = "http://aarch64-darwin-${n}.lan:9184"; })
    [ "6" "7" "8" "9" "a" "b" "c" "d" "e" "f" ];
  agent = { enable = true; name = "x8664-linux-0"; };
  nginx = { enable = true; virtualHost = "ci.zw3rk.com"; };
};
```

## License

Apache License 2.0; see [LICENSE](LICENSE). The vendored ECharts
(`hydrometer-ui/static/vendor/echarts.min.js`) is also Apache-2.0.
