# MCP benchmarks

Tasks for measuring whether the `learn` topics and the server instructions are enough for an agent with no other context to get a job done, in few tokens and few turns. Iterate on `docs/mcp/learn` against these.

## Layout

```
docs/mcp/benchmarks/
  <task>/
    prompt.md          the task, given verbatim to the worker agent
    setup.md           optional: a transcript that prepares the starting codebase
    judges/
      <judge>.md       a prompt for a judge agent; there may be several per task
```

## The codebase

`prepare.sh [<task>]` prints the directory of a fresh codebase (project `scratch`, branch `main`, `@unison/base` installed in `lib`). The base codebase is built once from `base-codebase.md` (it needs the network the first time, about 15 seconds) and cached under `$TMPDIR`, keyed by the hash of that file, so changing the pinned library version rebuilds it. Each call then copies it, which takes milliseconds, so runs never share state. If the task has a `setup.md`, it is run on the copy first.

``` 
UNISON="$(stack exec -- which unison)" docs/mcp/benchmarks/prepare.sh list-pick
```

Point the worker's MCP server at the printed directory with `unison -C <dir> mcp`, and copy the directory again for each judge. Set `UNISON` to the executable under test; the default is `unison` on the PATH.

## Protocol

1. **Prepare** a fresh codebase with `prepare.sh <task>`.
2. **Work.** A fresh agent with only the MCP server connected gets `prompt.md` and nothing else. It is not told about `learn` topics or the repository. Record its tool calls, the tokens it used, and its turns.
3. **Judge.** For each file in `judges/`, copy the codebase the worker produced and give it to a fresh agent, with the judge file as its prompt. A judge sees the codebase and its prompt, not the worker's conversation.
4. **Score.** Every judge ends its reply with a line that is exactly `PASS` or `FAIL`, after a short paragraph of reasons. A judge may define more ratings: the `reuses-existing` judges end with `GREAT` (the exact alias), `PASS` (a wrapper that could be eta reduced, partial credit) or `FAIL`. The task passes when all its judges pass.

Metrics worth recording per run: pass/fail per judge, the worker's turns, tool calls, `learn` calls, failed transcript stanzas, and total tokens (the server instructions and every `learn` reply count).

Run each variant several times, since a single run is noise. Keep some tasks out of the loop (don't tune topics against them) to check for overfitting.

## Writing a judge

- Say exactly what to check and what to run. A check that a command can settle (`ls` of the tests, `dependencies`, comparing `crypto.hash` of two functions) is better named explicitly than left to the judge's discretion.
- Name the topics the judge may read, if any. A judge that tests whether the documentation stands on its own must not read the topics that teach how to write it.
- End with: "Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL."

## Tasks

- `list-pick`: write a small documented, tested function that returns the elements (not the indices) selected by a list of Booleans. Judges: `implementation` (type, values not indices, edge cases, tail recursive, long list), `docs-quality`, and `tests`.
- `list-xf`: `List.xf`, a function that is `List.map` under another name. Don't read the source of `List` functions. Judges: `implementation` (tests exist and pass, tail recursive, following helpers through `dependencies`, function first, 100 elements) and `reuses-existing` (is it `List.map`, by comparing hashes: GREAT for the alias, PASS for a wrapper, FAIL for a new implementation).
- `list-sift`: the same for `List.sift`, which is `List.filter`.
- `list-xf-keep`: the same for `List.xfKeep`, which is `List.filterMap`.
- `list-run-lengths`, `list-neighbors`, `list-running-totals`, `list-merge-sorted`: list tasks. The first three have a `reuses-existing` judge (a base function behind them); merge has an `efficiency` judge.
- `map-word-counts`, `map-invert`, `map-add-counts`, `map-group-by-length`: `Map` tasks with `behavior`, `tests` and `reuses-existing` judges.
- `euler-factorial-digits`, `euler-nth-prime`: Project Euler style; `Natural` needed for the first, an `efficiency` judge for the first.
- `types-shape-area`, `types-person-record`: a data type and a record.
- `text-extract-numbers`, `text-parse-log-line`: text patterns (`Pattern`); the reuse judge rates `Pattern` use GREAT.
- `abilities-coro`, `abilities-collect-log`: new abilities with handlers; an `ability-polymorphic` judge checks the signature lets other abilities through.
- `abilities-random-seeded`, `abilities-store-label`: using base's `Random` and `Store`; an `ability-use` judge.
- `errors-parse-all`, `errors-safe-average`: error handling with `Either`, `Exception` and `Abort`; an `errors` judge.
- `threads-slow-squares`, `threads-first-done`: structured concurrency with `IO.Threads`; they need the IO transcript tool.

Judges named `behavior` run the function on inputs the prompt doesn't show. `ideas.md` has the reasoning behind the set.
