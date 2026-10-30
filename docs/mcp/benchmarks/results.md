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
