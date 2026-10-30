# Benchmark harness

Scripts that run the benchmark tasks with context-free worker agents and judge agents (see `../README.md` for the protocol).

- `env.sh`: where things live. `BENCH_ROOT` (default `~/.cache/unison-mcp-bench`, outside `/tmp` so it survives a reboot) holds `bin/unison` (the frozen executable under test: copy yours there before a round), `codebases/` (cache for `prepare.sh`) and `bench/<run>/` (one directory per run: its codebase, the worker's transcripts in `agent/`, `calls.log`, the judges' copies).
- `mcpd.py`: one MCP server per run behind a local socket; exits after 15 minutes idle (`IDLE_SECONDS`), so finished runs don't hold memory. `ask.py` is the client the agents use (`learn`, `transcript <tool> <file>`) and restarts the server if it is gone.
- `wbegin.sh <run> <task>`: fresh codebase, starts the server, prints the worker's whole instructions (client usage, the server's tool definitions and instructions, the prompt). The worker agent is told only to run it.
- `jbegin.sh <run> <task>`: stops the worker's server, gives each judge a copy of the codebase and the worker's transcripts (`{{WORKER_TRANSCRIPTS}}`, `{{CALLS_LOG}}`), prints the judge jobs. `cleanup.sh <run>` stops a run's servers.
- `metrics.py <round>`: calls, `learn` calls, failed stanzas and the topics read, per worker run. `ledger.py <round>`: ratings per run from the `judged.json` each judge agent saves, and the reasons for failures.
- The rounds were run as a Claude Code workflow (`pool` of chains: worker agent, then judge agent): see `report.md` for how, and for the lessons (memory, long calls).
