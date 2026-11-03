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

## Rounds 17 and 18: `workflow-merge-long`, and how workers read a generated file

The task's merge writes a 638-line file with its 3 conflicts in the first 30 lines. Six runs (r17, judged) passed every judge, GREAT on `approach`. Four more (r18) and four of `workflow-merge-conflict`, whose file is 17 lines, ran after two topic fixes and weren't judged.

| | runs | searched for the markers first | printed the whole file | looked at the size first |
|---|---:|---:|---:|---:|
| long file, r17 | 6 | 5 | 1 | 6 |
| long file, r18 | 4 | 1 | 3 | 1 |
| short file, r18 | 4 | 0 | 4 | 0 |

A run that searched read about 2K to 6K characters of the file, and one that printed it read 16K. Nothing lost the dependents, however it read the file. r17 against r18 is a large swing with no change that explains it, so treat 6 of 10 as the rate, not either round. Fixed from r17: `update` in a later transcript than the `load` (`ucm/temporary-branch`, `ucm/update`) and a copied `myproject/alice>` prompt (`ucm/merge`). In r18 the only failed call in any run is the merge itself, which reports a conflict as an error.

### Round 19: the merge reply says how long the file is

The reply now says `The file is 638 lines long and has 3 conflicts`, instead of `The file can be long`.

| | runs | searched for the markers first | printed the whole file |
|---|---:|---:|---:|
| long file, before (r17, r18) | 10 | 6 | 4 |
| long file, with the size (r19) | 8 | 8 | 0 |
| short file, with the size (r19) | 2 | 0 | 2 |

The long runs read 2.4K to 5.8K characters of the 15K file. The short runs still print their 17 lines, which is the right choice. r19 wasn't run through the judges; a direct check of each run's end state is in the log.


## Round 20: the refactoring tasks, after the work-file recipes (2 November 2026)

Sonnet workers run as Claude Code subagents (one per task, in parallel, each with its own server from `wbegin.sh`), Sonnet judges from `jbegin.sh`. The binary has the MCP server writing real files, `rewrite` serving a work file (plain names for the file's own definitions, the rules below a fold), the `ucm/rewrite` topic and the step-by-step `skills/refactor`. Three new tasks: `refactor-rename`, `refactor-change-implementation`, `refactor-constructor-fields`; `method` judges on every refactoring task.

| task | outcome judges | method | what the worker did |
|---|---|---|---|
| `refactor-rename` | pass | GREAT | `move Stock.restock Stock.replenish`, `move Sku Item`, `move Item.Sku Item.Item`, as the topic says; 7 calls |
| `refactor-change-implementation` | pass | GREAT | new `Text.slug` and a test in one block, `update`, `test`; never sent a dependent |
| `refactor-constructor-fields` | pass | GREAT | the whole recipe: `edit.to` with the dependents, `Reading_v2` with suffixed constructors, one rule block, `rewrite w1.u:rules.r1 w1.u`, read the file, stripped `_v2`, `load`, `update` |
| `refactor-swap-arguments` | pass | GREAT | the recipe through `rewrite`, but then retyped the rewritten definitions by hand "since I couldn't read that file": the reply named it relative to the server's directory, not the agent's |
| `refactor-add-argument` | pass | PASS | `edit.to` with the dependents, then edited the six uses in the file by hand, no rule |
| `refactor-add-constructor` | pass | GREAT | `dependents Shape`, `edit.to work.u Shape 1-12`, cases added by hand (a rewrite can't add one); tried `view work.u` once |
| `refactor-ability-operation` | pass | PASS | `find.all Journal`, viewed the handlers, one block with everything; lost a test's second check and recovered it from `history` with `fork` |

7 / 7 pass. Changes made from this round: the `rewrite` reply gives the file's absolute path under the MCP server (the swap worker's only miss), `ucm/rewrite` says to read the file rather than retype it, the add-argument recipe says to write the rules even for a few uses, and the `method` judges of the constructor and operation tasks count a `find` of the namespace before the change (the ability worker's `find.all Journal` is a fair way to find the handlers).

## Round 21: the same, after the round 20 changes

Sonnet workers, one run each (two for `add-argument`), judged by Sonnet. The commands come from the workers' transcripts (`agent/*.md`), not their reports.

| run | outcome judges | method | what the worker ran |
|---|---|---|---|
| `refactor-add-argument` (a) | pass | GREAT | `edit.to work.u bullets`, `dependents`, `edit.to work.u 1-`; a `load` that failed on a duplicate `bullets.doc` (no `load` between the two `edit.to`s, so nothing was deduplicated); `rewrite work2.u:rules.r1,rules.r2 work2.u`, read the file, stripped `_v2`, `load`, `update`, `test` |
| `refactor-add-argument` (b) | pass | GREAT | the same recipe with no detour, 5 calls |
| `refactor-swap-arguments` | pass | GREAT | the recipe, and this time it read the file `rewrite` wrote (absolute path in the reply) |
| `refactor-add-constructor` | pass | GREAT | `view Shape`, `dependents Shape`, `find-in Shape`; `rules.r1 = @rewrite case Shape.Circle ==> Shape.Circle` and `rewrite.find rules.r1` (listed `area`, `corners`, `hasCurve`, `perimeter`, `scale`); `edit.to work.u` with the type, those, and the wildcard ones and tests too; cases by hand; `update`, `test` |
| `refactor-ability-operation` | pass | GREAT | `find.all Journal`, `edit.to work.u` with the ability, handlers, `audit` and tests; two `update`s failed because `Inventory.tests.audit` in the file calls `Inventory.receive` of the branch, which uses the old `Journal`; `dependents Journal`, brought `receive` in unchanged; `update`, `test` |

5 / 5, all GREAT. What this round shows: with the one-line nudge, both `add-argument` workers wrote rules instead of editing the uses by hand (0 of 1 in round 20); the absolute path fixed the swap worker's retyping; the `case Shape.Circle ==> Shape.Circle` search was used as described. Two frictions remain: `edit.to` doesn't deduplicate against the target file unless it was `load`ed last (a duplicate `bullets.doc` cost the (a) worker a call), and a work file that holds a caller of a branch function which uses the old type can't typecheck (two failed `update`s for the ability worker); `skills/refactor` now says which definitions belong in the file and which don't (a function that doesn't match on the type needs no change).

## Round 22: call counts, after `edit.to` deduplicates against its target file

The aim of this round was the path, not the pass rate: how many calls each worker made, and how many failed. Changes before it: `edit.to` leaves out the definitions its target file already has (parsed from disk, whether or not it was the last file loaded) and says `Nothing to add` when that is everything; `ucm/rewrite` sends plain "every use" searches to `dependents` and keeps `rewrite.find` for shapes; `skills/refactor` says a function that doesn't match on the type needs no change (and which definitions do belong in the work file) and that `update` takes no argument. Sonnet workers, Sonnet judges; the commands are from the transcripts.

| run | calls (failed) | r21 | method | where the calls went |
|---|---|---|---|---|
| `refactor-add-argument` (a) | 8 (1) | 11 (2) | GREAT | explore and `edit.to` in one call; repeated that call once; rules in a `unison` block and `rewrite scratch.u:rules.r1,rules.r2 scratch.u`; `load`, `update`, `test` |
| `refactor-add-argument` (b) | 11 (1) | 13 (4) | GREAT | the recipe, but `load` and `rewrite` in separate calls, then an `update` in a call with no `load` (failed), then both together |
| `refactor-ability-operation` | 7 (1) | 8 (2) | GREAT | `dependents Journal`, one `edit.to`, one block, `update`; the old test's expectation then had to change (the prompt makes the collecting handler record `WARN: ...`), one more round |
| `refactor-constructor-fields` | 11 (2) | 12 (2), r20 | GREAT | thorough exploration (two read-only calls), left `Alert.tests.text` out of the file, so `update` stopped on `update-main`; finished through the temporary-branch flow |

4 / 4, all GREAT, and a quarter fewer calls than the same runs in round 21 (37 against 44 in the comparable four, 5 failures against 10). What remains is the agents' own slips, each costing one call: a `load` in one call and `update` in the next (the topic says to send them together; an `update <file>` that loads and updates in one would remove the trap), the same `edit.to` sent twice, a dependent left out of the work file. The harness rule that workers write only under `agent/` made two of them rebuild the work file instead of editing the one `edit.to` wrote; `wbegin.sh` now lets them edit the files that replies name.

## Round 23: all seven tasks, after `update <file>`, the in-place rewrite and the carried `load`

Sonnet workers and judges, one run per task; commands from the transcripts. Changes before it: `update <file>` (loads and updates in one), a `load` carrying over to the next transcript under the MCP server, `rewrite` rewriting its file target in place with the version before saved under `rewrites/`, `dependents` help saying "direct", a hint when `view` is given a `.u` file, and the topic edits agreed on 2 November (which definitions belong in the work file, tests whose expectations change, docs, `test` needs no IO, a path you can edit for `edit.to`).

| task | judges | method | calls (failed) | what the calls went on |
|---|---|---|---|---|
| `refactor-rename` | pass | GREAT | 4 (0) | three `move`s; the fewest calls yet |
| `refactor-change-implementation` | pass | GREAT | 5 (0) | `edit.to agent/work.u Text.slug`, the new definition, `update agent/work.u` in one call |
| `refactor-add-constructor` | pass | GREAT | 10 (3) | tried `rewrite.find` with the rule written inline (1 failed call); `rewrite.find :matchesOnCircle` found the five matchers; two `update`s failed because the tests in the file call `totalArea` and `isSquare` of the branch under the old `Shape` |
| `refactor-ability-operation` | pass | GREAT | 10 (3) | `rewrite.find` on `case Journal.entry`; its own new test had the wrong expectation first, then the old test's expectation |
| `refactor-add-argument` | pass | GREAT | 11 (2) | the recipe; an `edit.to` sent with no names (1 failed call); its strip script matched `rules.r1` in the header comment of the rewritten file and emptied it, restored from the saved version under `rewrites/` |
| `refactor-swap-arguments` | pass | GREAT | 10 (0) | the recipe; the same header-comment accident, same recovery |
| `refactor-constructor-fields` | pass | GREAT | 13 (2) | the recipe; the same header-comment accident; `Alert.text` left out of the file, found through the `Alert#njpksb06el` error |

7 / 7, all GREAT; the rename and change-implementation runs are now 4 and 5 calls. Three workers emptied their own work file with a strip script that looked for `rules.r1`, which the header comment of the rewritten file also mentioned; all three recovered from the saved version, which the in-place design made possible, and the rewritten file now gets no header. Three runs lost calls to the "two types of the same name" error (a branch definition using the old type called from the file); the type and ability mismatch errors now carry a note saying what it is and what to do. One worker wrote the rule inline in `rewrite.find`, which doesn't parse; worth allowing.

## Round 24: the three tasks that hit the "two types of the same name" error, after `load` pulls the affected definitions in

Change before it: when a file redefines something and uses a definition of the codebase that uses the old version, `load` makes that definition part of the file (unchanged, with a line saying so: `Also typechecking, from the codebase: ...`) and `update` saves it rebuilt; `rewrite` writes no header into a file rewritten in place. Sonnet workers and judges; commands from the transcripts.

| task | judges | method | calls (failed) | r23 | what happened |
|---|---|---|---|---|---|
| `refactor-add-constructor` | pass | GREAT | 7 (0) | 10 (3) | `rewrite.find` on `case Shape.Circle`, `edit.to` of the type and the five matchers only; `load` pulled `Shape.isSquare` in for the test that calls it; `update work.u`, `test` |
| `refactor-ability-operation` | pass | GREAT | 8 (2) | 10 (3) | `dependents Journal`, one `edit.to`, `update work.u` (the two failures: an argument-order slip in the first call, and the old test's expectation, fixed in one more call); `Inventory.receive` never needed a thought |
| `refactor-constructor-fields` | pass | GREAT | 7 (0) | 13 (2) | the recipe end to end, 4 transcript calls; `Alert` and `Alert.text` came along by themselves |

3 / 3, all GREAT, 22 calls against 33 in round 23 for the same tasks, and no call lost to the error the change removes.

## Round 25: all 51 tasks, on the branch as of `7449c6275` (2 November 2026)

Sonnet workers and judges, one run per task, every task in the set (the 47 of round 15 plus the four refactoring tasks added since). Changes since round 15 that these runs exercise: the MCP server writing real files, `update <file>`, the carried `load`, `rewrite` in place, `load` pulling in the affected definitions, and the refactoring topics. The traces of every run were read for the failed calls (`compare.py r15 r25`, and the failed calls' inputs and replies).

48 / 51 pass. The three misses are each one judge: `text-parse-log-line/tests` (no test with punctuation in the message), `threads-slow-squares/docs-quality` (the doc's example is `run slowSquares [1, 2, 3]`, which `run` rejects: it takes a delayed computation), and `workflow-merge-cancel/outcome` (the worker added an assertion to Bob's test while bringing it over, so its hash changed). Six of the seven refactoring tasks are GREAT on `method`; `refactor-constructor-fields` is PASS, the worker having read the recipe and then made the eleven edits by hand (8 calls, against 7 with the rewrite in round 24).

Against round 15 (the same 47 tasks): 363 calls against 341, 181 `learn` calls against 163, 37 failed calls against 57, and 21 replies written to a file against 104 (the longer replies are now short enough to return inline).

| task | judges | notes | calls (failed) | `learn` |
|---|---|---|---|---|
| `abilities-collect-log` | pass |  | 6 (0) | 4 |
| `abilities-coro` | pass |  | 9 (0) | 5 |
| `abilities-random-seeded` | pass | reuses-existing GREAT | 10 (2) | 6 |
| `abilities-store-label` | pass | ability-use GREAT | 7 (1) | 5 |
| `errors-parse-all` | pass | reuses-existing GREAT | 4 (0) | 2 |
| `errors-safe-average` | pass | reuses-existing GREAT | 6 (1) | 4 |
| `euler-factorial-digits` | pass |  | 8 (0) | 5 |
| `euler-nth-prime` | pass |  | 8 (0) | 6 |
| `list-merge-sorted` | pass |  | 9 (1) | 5 |
| `list-neighbors` | pass | reuses-existing GREAT | 7 (1) | 3 |
| `list-pick` | pass |  | 6 (0) | 4 |
| `list-run-lengths` | pass | reuses-existing GREAT | 8 (0) | 4 |
| `list-running-totals` | pass | reuses-existing GREAT | 9 (0) | 6 |
| `list-sift` | pass |  | 8 (0) | 6 |
| `list-xf` | pass |  | 7 (1) | 4 |
| `list-xf-keep` | pass |  | 7 (1) | 4 |
| `map-add-counts` | pass |  | 6 (0) | 5 |
| `map-group-by-length` | pass | reuses-existing GREAT | 8 (1) | 4 |
| `map-invert` | pass | reuses-existing GREAT | 8 (0) | 5 |
| `map-word-counts` | pass | reuses-existing GREAT | 9 (0) | 6 |
| `refactor-ability-operation` | pass | method GREAT | 5 (0) | 2 |
| `refactor-add-argument` | pass | method GREAT | 11 (2) | 3 |
| `refactor-add-constructor` | pass | method GREAT | 7 (0) | 2 |
| `refactor-change-implementation` | pass | method GREAT | 7 (0) | 3 |
| `refactor-constructor-fields` | pass | method PASS | 8 (1) | 3 |
| `refactor-rename` | pass | method GREAT | 3 (0) | 1 |
| `refactor-swap-arguments` | pass | method GREAT | 8 (0) | 4 |
| `text-extract-numbers` | pass | reuses-existing GREAT | 8 (0) | 7 |
| `text-parse-log-line` | fail: tests | reuses-existing GREAT | 8 (1) | 6 |
| `threads-first-done` | pass | reuses-existing GREAT | 10 (3) | 5 |
| `threads-slow-squares` | fail: docs-quality | reuses-existing GREAT | 8 (3) | 4 |
| `types-person-record` | pass | record-style GREAT | 15 (5) | 6 |
| `types-shape-area` | pass |  | 6 (0) | 5 |
| `workflow-delete-with-dependents` | pass |  | 9 (0) | 3 |
| `workflow-feature-branch` | pass |  | 10 (1) | 6 |
| `workflow-http-json-server` | pass | library-choice GREAT | 18 (3) | 2 |
| `workflow-json-encode-record` | pass | library-choice GREAT | 10 (0) | 4 |
| `workflow-lib-install-fresh` | pass | library GREAT, tests-and-docs GREAT | 10 (1) | 3 |
| `workflow-lib-install-upgrade` | pass |  | 5 (0) | 2 |
| `workflow-lib-upgrade-conflict` | pass |  | 7 (1) | 2 |
| `workflow-lib-upgrade-two` | pass |  | 7 (1) | 2 |
| `workflow-merge-cancel` | fail: outcome |  | 8 (1) | 4 |
| `workflow-merge-clean` | pass |  | 3 (0) | 1 |
| `workflow-merge-conflict` | pass | approach GREAT | 4 (1) | 2 |
| `workflow-merge-long` | pass | approach GREAT | 7 (1) | 3 |
| `workflow-merge-silent-combine` | pass | approach GREAT | 6 (2) | 2 |
| `workflow-merge-type-break` | pass | approach GREAT | 4 (1) | 2 |
| `workflow-undo-update` | pass |  | 6 (2) | 1 |
| `workflow-update-clean` | pass |  | 4 (0) | 1 |
| `workflow-update-multiround` | pass |  | 6 (0) | 3 |
| `workflow-update-type-conflict` | pass |  | 10 (0) | 4 |

**What the failed calls went on,** by cause, with what changed because of it:

- *A doc with a `{` in its prose* (`map-group-by-length`, `list-neighbors`): a map shown as `{1 -> [...]}` in a doc is read as a link, and the error ("I was surprised to find a newline here") doesn't say so. `lang/documentation` now says that a brace in prose starts a link.
- *A wrapper instead of an alias* (`list-xf`, `list-xf-keep`, `list-sift`, `map-add-counts`): all four reuse tasks ended PASS, not GREAT, on `reuses-existing`, because the worker wrote `List.xf f xs = List.map f xs`. `skills/reuse` offered that form in a parenthesis; it now says to define the name with no parameters, so that it is the same definition.
- *`Int.fromNat`* (`errors-safe-average`, `threads-first-done`, 3 calls): the right name is `Nat.toInt`, which `lang/numbers` has, and the error's "similar names" list (`Int.fromText`, `Nat.fromInt`) doesn't include it, although `find.all : Nat -> Int` does. Not changed: see the ideas below.
- *An ambiguous operator in a helper with no signature* (`threads-slow-squares`, 2 calls, `n * n` in a local `square n`) and *`<` on a type variable* (`list-merge-sorted`): both are in `lang/operators`, which no worker in the round read. `lang/collections`, which they do read, now points at `Universal.lt` and `Universal.ordering` for generic comparison.
- *The record type's field accessor* (`types-person-record`, 5 calls in all): `Person.age` doesn't exist for a `type Person = Person { ... }`, and `compare` is ambiguous (`Universal.compare` gives an `Int`; `Universal.ordering` is the `Ordering` one, found by `find.all : a -> a -> Ordering`). `lang/records` now says the new form generates no accessors.
- *A `docExample` without `do`* (`abilities-random-seeded`, 2 calls): already covered by `lang/documentation`, which the worker had read.
- *The new doc written before the `_v2` strip* (`refactor-add-argument`, 2 calls): a `bullets.doc` calling `bullets` with the new argument, while `bullets` in the file was still the old version. `skills/refactor` step 5 now says to write the doc after the strip.
- *`project.current`* (`list-xf`, and two judges): not a UCM command, and the reply was that it "needs `transcript-all`". The reply now says the command doesn't exist.
- *Guessed names* (`workflow-undo-update`, `workflow-merge-silent-combine`, `workflow-http-json-server`): `view` of a name that isn't there, three or four calls across the round. Not changed.
- *The rest*: library-use type errors in the HTTP service (2), a char written `'a'` (1), `open` for `use` (1), `List.foreach` with the list first (1), and the eight merge and upgrade conflicts, which are errors by design.

Also in the harness: three workers misread the client's `transcript <tool> <file>` usage line and lost a call to it; `wbegin.sh` now gives an example.

**Ideas not done.** (1) When a name isn't found and its expected type is known (`Int.fromNat : Nat -> Int`), the error could list the terms in the codebase with that type, as `find.all : Nat -> Int` would: the typechecker only looks at similar names, so this would go in the output layer with a codebase query. (2) `view` of a namespace (`view Report.render.tests`) could say it is a namespace and suggest `ls`. (3) `workflow-merge-cancel`: the judge wants Bob's test at its own hash; a worker that reads it with `view` and types it back in is one edit away from changing it, and nothing in `ucm/merge` says to bring it over unchanged.

## Round 26: the give-away examples replaced, and eight held-out tasks (3 November 2026)

Before this round, a read of the topics against the tasks found that rounds 1 to 15 had put several tasks' own solutions into the topics as examples: `List.xf = List.map` and the `find.all : (a -> b) -> [a] -> [b]` search, a list of base functions each glossed in its task's words (`slidingPairs`, `groupConsecutive`, `scanLeft`, `filterMap`, `groupBy`), the `Tree.label` helper signature in `lang/mutable`, `pickTwo` (a seeded shuffle and `take`) in `lang/random`, `findOrNone x = Either.toOptional (Exception.catch do find x)` in `lang/errors`, the `parMap`-with-a-sleep example in `lang/concurrency`, "or a coroutine" in `lang/abilities`, and `List.pick` as the `docExample`. Each lesson was kept and its instance changed to one no task asks for: a search by type that finds `List.chunk` and `List.slidingWindow`, a loop-finding method ("say what the loop does in a word and search by it, then by type") with `List.partition` as the example and `List.len = List.size` as the alias, a JSON walk as the `Ref` helper, dice rolls as the seeded function, a `Throw` parser with an `Either` twin, `Threads.both` on two slow texts, and `List.take` in the `docExample`. The README says to keep tasks out of the tuning loop, and none had been, so eight held-out tasks were added in the same areas (see the README): they were written before this round and never tuned against.

Sonnet workers and judges, one run per task, 59 tasks. 57 / 59 pass. The two misses: `abilities-coro` (the worker read the whole reply stream with `Stream.toList` before feeding, so the lazy-pull check fails; it passed in round 25, and the topic edit that touched it only dropped the words "or a coroutine") and `workflow-merge-cancel/outcome` (the worker again added an assertion to Bob's test while typing it back in; there is no way to copy a definition from another branch by name, since `view /bob:Text.slug` and `alias.term /bob:Text.slug` both reject the branch prefix).

**The held-out tasks**, which measure whether the topics carry over to problems they weren't written for:

| task | judges | notes | calls (failed) | round 25 |
|---|---|---|---|---|
| `list-chunks` | pass | reuses-existing GREAT | 8 (0) |  |
| `map-merge-max` | pass | reuses-existing GREAT | 8 (0) |  |
| `random-sample` | pass | reuses-existing GREAT | 8 (2) |  |
| `mutable-word-ids` | pass | state-use GREAT | 7 (0) |  |
| `threads-timed-pair` | pass | reuses-existing GREAT | 4 (0) |  |
| `text-parse-kv` | pass | reuses-existing GREAT | 9 (0) |  |
| `abilities-counter` | pass |  | 7 (0) |  |
| `errors-parse-port` | pass | errors GREAT | 10 (4) |  |

8 / 8 pass, and six of the eight are GREAT on the judge that rates the method. `abilities-counter` has no GREAT grade (its `ability-polymorphic` judge is pass or fail), and `mutable-word-ids` used a single `List.foldLeft` with the `Map` as the accumulator, which the judge's rules rate GREAT although the judge wrote PASS and asked. So the lessons transfer: the workers found `List.chunk` and `Map.unionWith` by searching, composed `Random.splitmix` with `Random.shuffle` and `List.take` for a sample (the example that used to be in the topic, now reinvented from the topic's description of the parts), used `Threads.both`, wrote the key-value grammar as a `Pattern`, wrote a stateful handler with the abilities passing through, and derived `parseOrNone` from `parse` with a handler.

**The 51 tasks of round 25**, on the rewritten topics:

| task | judges | notes | calls (failed) | round 25 |
|---|---|---|---|---|
| `abilities-collect-log` | pass |  | 8 (1) | 6 (0) |
| `abilities-coro` | fail: ability-polymorphic, behavior |  | 7 (1) | 9 (0) |
| `abilities-random-seeded` | pass | reuses-existing GREAT | 10 (3) | 10 (2) |
| `abilities-store-label` | pass |  | 10 (3) | 7 (1) |
| `errors-parse-all` | pass | reuses-existing GREAT | 6 (0) | 4 (0) |
| `errors-safe-average` | pass | reuses-existing GREAT | 10 (4) | 6 (1) |
| `euler-factorial-digits` | pass |  | 6 (0) | 8 (0) |
| `euler-nth-prime` | pass |  | 8 (1) | 8 (0) |
| `list-merge-sorted` | pass |  | 8 (1) | 9 (1) |
| `list-neighbors` | pass | reuses-existing GREAT | 9 (0) | 7 (1) |
| `list-pick` | pass |  | 7 (0) | 6 (0) |
| `list-run-lengths` | pass | reuses-existing GREAT | 11 (2) | 8 (0) |
| `list-running-totals` | pass | reuses-existing GREAT | 7 (0) | 9 (0) |
| `list-sift` | pass | reuses-existing GREAT | 7 (0) | 8 (0) |
| `list-xf` | pass | reuses-existing GREAT | 7 (1) | 7 (1) |
| `list-xf-keep` | pass | reuses-existing GREAT | 5 (1) | 7 (1) |
| `map-add-counts` | pass | reuses-existing GREAT | 8 (0) | 6 (0) |
| `map-group-by-length` | pass | reuses-existing GREAT | 9 (0) | 8 (1) |
| `map-invert` | pass | reuses-existing GREAT | 7 (0) | 8 (0) |
| `map-word-counts` | pass | reuses-existing GREAT | 9 (0) | 9 (0) |
| `refactor-ability-operation` | pass | method GREAT | 9 (3) | 5 (0) |
| `refactor-add-argument` | pass | method GREAT | 11 (2) | 11 (2) |
| `refactor-add-constructor` | pass | method GREAT | 9 (1) | 7 (0) |
| `refactor-change-implementation` | pass | method GREAT | 8 (2) | 7 (0) |
| `refactor-constructor-fields` | pass | method GREAT | 7 (0) | 8 (1) |
| `refactor-rename` | pass | method GREAT | 4 (0) | 3 (0) |
| `refactor-swap-arguments` | pass | method GREAT | 8 (0) | 8 (0) |
| `text-extract-numbers` | pass | reuses-existing GREAT | 7 (0) | 8 (0) |
| `text-parse-log-line` | pass | reuses-existing GREAT | 12 (2) | 8 (1) |
| `threads-first-done` | pass | reuses-existing GREAT | 12 (2) | 10 (3) |
| `threads-slow-squares` | pass | reuses-existing GREAT | 6 (1) | 8 (3) |
| `types-person-record` | pass | record-style GREAT | 13 (4) | 15 (5) |
| `types-shape-area` | pass |  | 9 (0) | 6 (0) |
| `workflow-delete-with-dependents` | pass |  | 7 (2) | 9 (0) |
| `workflow-feature-branch` | pass |  | 10 (1) | 10 (1) |
| `workflow-http-json-server` | pass | library-choice GREAT | 29 (13) | 18 (3) |
| `workflow-json-encode-record` | pass | library-choice GREAT | 10 (0) | 10 (0) |
| `workflow-lib-install-fresh` | pass | library GREAT, tests-and-docs GREAT | 12 (1) | 10 (1) |
| `workflow-lib-install-upgrade` | pass |  | 7 (0) | 5 (0) |
| `workflow-lib-upgrade-conflict` | pass |  | 9 (2) | 7 (1) |
| `workflow-lib-upgrade-two` | pass |  | 10 (3) | 7 (1) |
| `workflow-merge-cancel` | fail: outcome |  | 13 (3) | 8 (1) |
| `workflow-merge-clean` | pass |  | 3 (0) | 3 (0) |
| `workflow-merge-conflict` | pass | approach GREAT | 3 (1) | 4 (1) |
| `workflow-merge-long` | pass | approach GREAT | 5 (1) | 7 (1) |
| `workflow-merge-silent-combine` | pass | approach GREAT | 4 (1) | 6 (2) |
| `workflow-merge-type-break` | pass | approach GREAT | 5 (1) | 4 (1) |
| `workflow-undo-update` | pass |  | 4 (0) | 6 (2) |
| `workflow-update-clean` | pass |  | 4 (0) | 4 (0) |
| `workflow-update-multiround` | pass |  | 8 (1) | 6 (0) |
| `workflow-update-type-conflict` | pass |  | 6 (0) | 10 (0) |

49 / 51 against 48 / 51 in round 25, with different misses (the two above, against the log-line tests, the slow-squares doc and the same merge-cancel). The refactoring tasks are 7 / 7 GREAT on `method` (6 / 7 in round 25: the constructor-fields worker used the rewrite this time). The three alias tasks (`list-xf`, `list-sift`, `list-xf-keep`) and `map-add-counts` are GREAT on `reuses-existing` where round 25 had four PASS wrappers: the reuse topic's "with no parameters" wording did that, and it did it with the `List.map` example gone.

Calls: 484 in all (8.2 per run), 242 `learn` calls, 71 failed calls, of which 15 are the same transcript sent again unchanged within seconds of its failure (a worker habit, not a harness retry: `ask.py` has none). On the 47 tasks that have a round 15 baseline: 423 calls against 394 in round 25 and 341 in round 15, 65 failed calls against 40 and 57. The extra failures sit in a few runs: the HTTP service (13, a hallucinated `Route.pickStatus` sent four times, then library type errors), `errors-safe-average` (4: a doc example of a raising function without `catch`, `{Exception}` for `{type Exception}`, then `->{type Exception}` in a signature, sent twice), `abilities-store-label` and `abilities-random-seeded` (a `docExample` without `do`, each sent twice), `refactor-ability-operation` (3: an anonymous `{{ }}` doc placed before a `test>` line binds to `>`, sent twice, then the changed test's old expectation), `types-person-record` and `text-parse-log-line` (`Person { name = "Ann" }` with `=`, each sent twice; the error says only that a `}` has no `{`). None of these is a topic that lost its example: `docExample`'s `do`, the record `:`, `{type X}` and `catch do` in doc examples are all in `lang/documentation` and `lang/records` already, and were in round 25.

**What to take from it.** The transfer result is the point of the round: the rewritten topics teach the same things without naming the answers, and the held-out tasks pass at the same rate as the tuned ones. The failed-call rate is noisier than the pass rate (a worker that resends an identical transcript doubles its own count), and the biggest single sinks are two syntax points that the topics state but workers miss on the first try: `docExample`'s `do` and the record constructor's `:`. Both have unhelpful errors ("1st argument has type Unit", "closing } without a matching {"), so the fix that would generalize is in the parser's messages, not in the topics. Also noted: the merge-cancel miss repeats because the only way to bring one definition over from another branch is to retype it, and the topic doesn't say to bring it over unchanged.

## Rounds 27 and 28: the refactoring tasks, after removing answers from the topics

**Why.** An audit of the `method` judges found that the rewrite and refactoring topics used examples that were the answers to three tasks: `Journal.entry` (ability operation), `Shape.Circle` (add constructor) and `Report.bullets` with `maxNat` (add argument). Those examples now use unrelated names (`Metrics.count`, `Token.Word`, `Price.format` with a currency symbol). The `method` judges now say that GREAT records following the method the topics teach, not finding it unaided.

**Round 27** (all seven refactoring tasks, Sonnet workers and judges, binary rebuilt with the new examples): every task had method GREAT and every other judge PASS, except `refactor-swap-arguments`, whose outcome judge said FAIL. Four callers had been rewritten to a lambda applied directly to an argument, `(b -> Log.line b "INFO") 7`, where the use should read `Log.line 7 "INFO"`. The behavior was right; the form was not. The cause was in the topic: its rules covered the full call, one argument and the bare function, so a use with two of three arguments given went through the one-argument rule.

**Topic change.** `skills/refactor` now says that a function of three or more arguments needs a rule for every count of arguments from all down to none, and to reduce a lambda applied directly to an argument after a rewrite (`(a -> f a x) y` is `f y x`). No benchmark names were used.

**Round 28** (swap-arguments, add-argument and constructor-fields again, since the change touches partial applications): all three have method GREAT and every other judge PASS. Swap-arguments now writes four rules, one per argument count, as the topic says.

| Task | Round 27 | Round 28 |
|---|---|---|
| refactor-rename | GREAT, all PASS | not rerun |
| refactor-add-constructor | GREAT, all PASS | not rerun |
| refactor-change-implementation | GREAT, all PASS | not rerun |
| refactor-ability-operation | GREAT, all PASS | not rerun |
| refactor-add-argument | GREAT, all PASS | GREAT, all PASS |
| refactor-constructor-fields | GREAT, all PASS | GREAT, all PASS |
| refactor-swap-arguments | GREAT, outcome FAIL | GREAT, all PASS |

**To take from it.** Seven of seven are now GREAT with every outcome PASS. That says the topics teach the method well enough for Sonnet to follow, and that the earlier rating did not depend on a leaked example. It does not say a worker would find a rewrite unaided, since the topics point to it. Round 27's agents used only file reads and the shell, with no other MCP tools.
