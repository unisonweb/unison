# Benchmark rounds, 30 October 2026: summary report

## In short

- **Sonnet passes 46 of 47 tasks in a full run (r11) and the one miss passes after a one-line topic change (r12).** The first full run on the topics as I found them (r7) was 47 of 47 for Sonnet and 39 of 47 for Haiku; the last (r11) was 46 of 47 and 45 of 47. One run per task and model per round, so single misses are noise as much as signal.
- The 47 tasks are all the task folders: the 16 you approved, 15 more single-function tasks (lists, maps), and the 16 workflow and refactoring tasks you reviewed. No task has been dropped.
- What moved the numbers was mostly **harness and judge problems, not topics**: workers that stopped to wait for a background library install (fixed by a line in the worker prompt), a judge that rejected documentation, and an unreliable machine setup (see "The crash"). The topics changes helped Haiku (reusing base functions, finding libraries) and one Sonnet task (upgrading two libraries in one command).
- Following your note, I did not keep tuning for Haiku once Sonnet was passing: its remaining misses are hand-written list loops (`List.map`, `List.filterMap`, `List.filter`, `List.scanLeft` exist in base) that vary from run to run.

## What was run

- A worker agent (Haiku or Sonnet, no context beyond the MCP server's tool definitions, its instructions and `prompt.md`, reached through the `harness/` proxy) did each task, then a Sonnet judge agent ran all of the task's judges on the resulting codebase. Rounds were run as Claude Code workflows with a bounded pool of chains.
- Rounds: r7 baseline; r8, r10 and r12 targeted re-runs of failing tasks; r9 and r11 full sweeps (94 runs each). About 14M subagent tokens per full sweep.

| round | change before it | Haiku | Sonnet |
|---|---|---|---|
| r7 | baseline | 39 / 47 | 47 / 47 |
| r9 | topics: `skills/library`, base list functions, finish checklist; `find` hint | 40 / 47 | 44 / 47 |
| r11 | harness: long calls in the foreground; judge fix; reuse and UCM-job pointers | 45 / 47 | 46 / 47 |
| r12 (targeted) | `ucm/upgrade`: several libraries in one command | `workflow-lib-upgrade-two` passes | same task and two library controls pass |

(r7 includes 63 results recovered from the workflow journal after the crash; their worker transcripts were lost, so I re-ran the failing ones to look at them.)

## What changed

**UCM** (all in the commits since `41da98db3`):
- `share.releases @user/project` (numbered, dated, up to 100) and `share.releases.detail` (release notes, by number or reference).
- A merged file under the MCP server starts with `-- The following definitions below were successfully merged at the line-level: ...` when both branches changed a definition and the changes combined without a conflict (two unit tests, checked on the silent-combine task).
- No fzf under the MCP server (a command with no arguments fails instead of waiting). `UCM_MERGETOOL` was already ignored there, and the transcript runner has no pager.
- `find` with no results says it doesn't search `lib` and suggests `find.all` (two transcripts regenerated). Plain `find` already falls back to a search of `lib` when the project has no match.
- Earlier in the session: `rewrite.find` / `rewrite` with a rewrites spec and targets, `edit.to` and friends with the `.u` guard, and the #5072 fix (see the previous report in this conversation).

**Learn topics:**
- New: `lang/numbers` (use `Nat`, `Nat.-` clamps at 0), `skills/library` (find, read, install and use a library from Share; build JSON with the library too; use `upgrade` for new versions), `skills/refactor` (`rewrite.find`/`rewrite`), exploring and cherry-picking history (in `ucm/history`).
- Instructions (read by every worker): search Share for a library when base lacks the capability; "define the requested name as the existing function"; a list of the UCM job topics (update, merge, upgrade...); document and test even when the prompt doesn't say so.
- `ucm/transcript` ends its bullets with a "before you say you are done" checklist; `lang/collections/list` names the base list functions for common shapes (sliding pairs, grouping, scans, `filterMap`); `ucm/upgrade` says not to `cancel` a stopped upgrade and to upgrade several libraries together; `ucm/share` documents the releases commands.

**Judges and tasks:** `workflow-merge-cancel` accepts documentation and extra tests (the new "document everything" guidance had made its judge fail good work); `workflow-lib-install-fresh` rates documentation GREAT and its absence PASS (your call). The benchmark folders were revised after your review earlier today (see `workflows.local.md`).

## What the failures were

- **Harness, not topics (the largest cause).** In r9 the workers of four network tasks stopped their turn to "wait for a background install": long `ask.py` calls were backgrounded by the agent's shell tool and nothing ever woke the agent. Fixed by telling the worker to run long calls in the foreground with a long timeout.
- **A judge too strict:** `workflow-merge-cancel` outcome failed workers that documented `Text.slug`.
- **Sonnet, one real topic gap:** `workflow-lib-upgrade-two` (one `upgrade` command, not one library at a time). Passes now in 3 of 3 runs (Sonnet twice, Haiku twice).
- **Haiku:** hand-written list loops (above), `workflow-http-json-server` without `sumService.doc` and sometimes with a hand-assembled JSON reply, `workflow-merge-cancel` rewriting Bob's test, `abilities-coro` without the ability variable on the producer (it passed in other rounds). I made two small changes for these (the library-output and finish-checklist lines) and stopped.
- **Calls and reads:** on average about 11.8 transcript and `learn` calls per run, 4.4 `learn` calls and 2.5 failed stanzas (r11). Workers nearly always read `ucm/transcript`, `testing` and the collection or ability topic for their area; Haiku rarely read `skills/reuse` or `lang/documentation`, which is why the always-read instructions and `ucm/transcript` carry the key rules now.

## The crash, and the setup

The machine crashed during the first full run: about 90 worker servers plus about 3 per judged run were up at once (each 60 to 200 MB, 16 GB total), and the benchmark scratch directory under `/tmp` was wiped, including my proxy scripts and the frozen binary. Rebuilt in `docs/mcp/benchmarks/harness/` with: data under `~/.cache/unison-mcp-bench` (survives reboots), servers that exit after 15 minutes idle and restart on demand, a pool of 5 to 7 chains instead of one chain per task, and a monitor that flags free memory under 15%. Free memory stayed at 62 to 66% in every later round.

## Caveats

- One run per task, model and round: a single failure is weak evidence. Haiku varies a lot between runs (for example `list-xf` failed in 3 of 5 runs and passed twice with no change in between).
- Judges are Sonnet agents; the wording of the new workflow judges had never run before r7, and some bad variants were never run through the full judge commands (listed in `workflows.local.md`). Sonnet's 47 of 47 at r7 says the judges are not very strict on the workflow tasks; I did not tighten them.
- Workers were Claude Code subagents, so they also got the repository's CLAUDE.md files as background, which a real client would not give them (this was also true in earlier rounds).
- The Sonnet r12 check was one or two runs per task: not a measurement of rate.

## Things for you

- Delete the cache when you are done with the harness: `rm -rf ~/.cache/unison-mcp-bench` (check first that nothing is running: `pgrep -f unison-mcp-bench`). It is several GB.
- Nothing since `41da98db3` is pushed. Commits are on `simple-mcp` locally.
- Open: `share.search` numbered results and `share.docs` (in `docs/mcp/todo.md`); the scratch-file ordering idea (`ideas.md`); `lib-install` tasks need the network and take minutes.
