# Results

One row per task, one column per worker model. A worker is a fresh agent that gets the server's tool definitions, its instructions and `prompt.md`, and nothing else (it reaches the server through a client script, and its calls are logged). Judges are Sonnet agents. A cell is `pass`, or `FAIL` with the judges that failed.

## Round 1: baseline (the `learn` topics as of `973434955`)

| task | Haiku | Sonnet |
|---|---|---|
| `abilities-coro` | pass | pass |
| `abilities-collect-log` | pass | pass |
| `abilities-store-label` | FAIL: tests (no case with both subtrees large). It threaded the counter by hand, which the judge now rates PASS (the prompt doesn't forbid it) | FAIL: tests, the same way |
| `abilities-random-seeded` | pass | FAIL: reuses-existing (hand-rolled a hash and sorted by it) |
| `threads-slow-squares` | pass | pass |
| `threads-first-done` | FAIL: docs-quality (didn't say how to run `Threads`) | pass |
| `errors-parse-all` | pass (recursion, not `Throw`) | pass (recursion, not `Throw`) |
| `errors-safe-average` | FAIL: reuses-existing (inverted: `average` built on `averageOrNone`), docs-present, tests (no empty-list test) | FAIL: reuses-existing (`averageOrNone` recomputes the average) |
| `text-extract-numbers` | FAIL: reuses-existing (character loop) | FAIL: reuses-existing (character loop) |
| `text-parse-log-line` | FAIL: reuses-existing (character loop) | FAIL: behavior (accepts `7:05` and `123:45`) |
| `types-shape-area` | pass | pass |
| `types-person-record` | pass (old record syntax) | pass (old record syntax) |
| `euler-factorial-digits` | pass | pass |
| `euler-nth-prime` | pass | pass |
| `list-xf` | FAIL: reuses-existing, implementation | FAIL: reuses-existing |
| `list-pick` | FAIL: implementation (`+:` with a non-tail call) | pass |
| **total** | 9 / 16 | 10 / 16 |

What the failures have in common, from the workers' transcripts:

- They write code by hand that base has: a `map` loop, a character loop where `Pattern` fits, a shuffle, a counter threaded through recursion where `Scope.ref` fits. Few of them searched the library, and the one that tried (`threads-slow-squares`, Haiku) used `find.all Scope` and guessed long names under `lib`, and made 130 calls, 80 of which failed.
- They read few topics: `ucm/transcript` and `testing` nearly always, the overview (`learn("")`) in a quarter of the runs, and `lang/collections`, `lang/records`, `lang/errors` almost never.
- They write documentation only when the prompt asks, and tests that cover the prompt's example and a few more, not each way the function can fail.
- Haiku used the deprecated record syntax and installed the base library a second time in two runs.
- Long replies are saved to a file, and the first round's worker prompt told them not to read other files. Most read the file anyway. (Fixed in the prompt for later rounds.)
- Judge changes made along the way: `abilities-store-label/ability-use` rates a hand-threaded counter PASS, not FAIL (`Scope.ref` is still GREAT), because the prompt doesn't say how to count.
- Other judge changes: `threads-slow-squares/behavior` no longer requires `Threads` in the type (handling it inside with `Threads.run` is fine).

## Round 2: new topics (`learn` as of `8baaba553`), Haiku only

12 / 16 (13 / 16 with the `ability-use` change). Passing now that failed before: `random-seeded` (reuses `Random.shuffle`), `list-pick` (`List.zip` and `List.filterMap`), `parse-log-line` (base `Text` functions), `first-done`, `person-record` (new record syntax, rated GREAT). Failing: `extract-numbers`, `safe-average`, `list-xf`, and `store-label` before the judge change. The failures were workers that never opened the topic for their area (`lang/text`, `lang/errors`, `lang/mutable`).

## Round 3: area-to-topic list in the instructions, Haiku only

15 / 16. Only `list-xf` fails: the worker noticed that `List.map` has the same shape but wrote its own `foldLeft`. `extract-numbers` and `parse-log-line` now use `Pattern` (GREAT), `store-label` uses `Scope.ref` (GREAT), `safe-average` derives `averageOrNone` from `average` with `Exception.catch` (GREAT), and `parse-all` uses `Throw.toEither` (GREAT). One `list-pick` worker created a project called `myproject` (copied from the examples in the topics), which the instructions now warn about. `first-done/behavior` was loosened like `slow-squares/behavior`.

## Round 4: alias guidance and the project-name warning, Sonnet (all 16) and Haiku (`list-xf`)

Sonnet 16 / 16. Haiku `list-xf`, the one failure of round 3, now passes: it wrote `List.xf f xs = List.map f xs`, which the reuse judge rates PASS (a wrapper that could be eta reduced). The reuse and style judges rate the Sonnet runs GREAT where it matters (`Throw.toEither`, `Pattern`, `Random.shuffle`, `Scope.ref`, `Threads.parMap`, `multiRace`, the new record syntax). Compared with the baseline, Sonnet moved from 10 / 16 to 16 / 16 and Haiku from 9 / 16 to 16 / 16, though the Haiku figure combines rounds 3 and 4, and each cell is one run.

Harness slips, so that nobody repeats them: judging a run while its worker is still going stops its server (it happened to `slow-squares` in round 1 and `list-pick` in round 4, and both were rerun), and the first Haiku `list-pick` run created a project named `myproject` from the examples in the topics.

## Round 5: confirmation run, Haiku, all 16 tasks on the final topics

15 / 16, from a fresh run of every task on one build. The failure is `abilities-coro`: the worker gave `Coro.feed` the type `'{Stream b} r1 -> '{g, Coro a b} r2 -> '{g, Stream a} ()`, with no ability variable on the producer, so a producer that uses another ability doesn't typecheck (the `ability-polymorphic` and `behavior` judges both caught it). It passed in rounds 2 to 4, so this is run-to-run variation, and `lang/abilities` now says to give every thunk argument the variable, with an example of a two-stream function.

### Round 5 follow-up: `abilities-coro` three more times, Haiku, after the `lang/abilities` change

3 / 3. All three runs gave both thunk arguments the ability variable (`'{g, Stream b} r -> '{g, Coro a b} s -> '{g, Stream a} ()`), and the `ability-polymorphic`, `behavior` (including the lazy producer check) and `tests` judges passed each time.

## Rounds 7 to 12: all 47 tasks, after the workflow and refactoring tasks were added

Every task (the 16 approved ones, 15 more single-function tasks, and the 16 workflow and refactoring tasks), each with a Haiku and a Sonnet worker, judged by Sonnet. Counts are tasks whose judges all passed, one run each. The harness is in `harness/` (scripts, a ledger and metrics tool); `report.md` has the full account.

| round | what changed before it | Haiku | Sonnet |
|---|---|---|---|
| r7 | baseline (`learn` topics as of `41da98db3`, new UCM commands) | 39 / 47 | 47 / 47 |
| r9 | topics for libraries from Share, base list functions, a finish checklist; `find` hint | 40 / 47 | 44 / 47 |
| r11 | harness told workers to run long calls in the foreground; judge fix; reuse and UCM-job pointers in the instructions | 45 / 47 | 46 / 47 |
| r12 | `ucm/upgrade` says to upgrade several libraries in one command | `workflow-lib-upgrade-two` passes | the same task, and two library controls, pass |

Sonnet's r9 misses were two network tasks whose worker stopped to wait for a background library install (a harness artifact, fixed in the worker prompt) and `workflow-merge-cancel` (the judge rejected documentation the worker added). Its only r11 miss, `workflow-lib-upgrade-two`, upgraded the two libraries one at a time and passes with one `upgrade` command once the topic said so. Haiku's remaining misses are hand-written list loops where base has the function (`List.map`, `List.filterMap`, `List.filter`, `List.scanLeft`), which vary from run to run.

## Rounds 13 to 16: a qualitative pass, from the MCP traces

Sonnet workers only, all 47 tasks (r16: three tasks). The aim was the path, not the pass count: every worker's calls and replies were read (`trace.jsonl`), and the topics and instructions changed where a run was wasteful. Prompts and judges did not change.

| round | what changed before it | calls | `learn` calls | failed calls | replies sent to a file |
|---|---|---:|---:|---:|---:|
| r13 | baseline | 446 | 201 | 85 | 123 |
| r14 | first edits (24 topics, instructions) | 350 | 166 | 60 | 104 |
| r15 | second edits | 341 | 163 | 57 | 104 |

r15 was judged: 44 / 47. Of the 3 misses, 2 are literal counts in `workflow-merge-conflict/no-collateral` and `workflow-merge-silent-combine/outcome` that a documentation term and an extra test exceed (the kind of judge problem `workflow-merge-cancel` had), and 1 is a real one, a hand-written recursion in `map-invert`, since addressed in `lang/collections/map`. r13 and r14 weren't judged.

r16 reran `map-invert`, `euler-nth-prime` and `refactor-swap-arguments` on the final topics, judged: 3 / 3, and `map-invert` went from FAIL to GREAT (`Map.fromListWith`). `refactor-swap-arguments` still does the swap by hand (`method` PASS).
