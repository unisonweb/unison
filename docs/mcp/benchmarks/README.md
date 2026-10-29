# MCP benchmarks

Tasks for measuring whether the `learn` topics and the server instructions are enough for an agent with no other context to get a job done, in few tokens and few turns. Iterate on `docs/mcp/learn` against these.

## Layout

```
docs/mcp/benchmarks/
  <task>/
    prompt.md          the task, given verbatim to the worker agent
    setup.md           optional: a transcript that prepares the starting codebase
    judges/
      <judge>.md                  a prompt for a judge agent; there may be several per task
      <judge>.deterministic.md    a transcript, run by judge.sh, with no LLM involved
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
3. **Judge.** Run `judge.sh <codebase> <task>` for the deterministic judges. For each other file in `judges/`, copy the codebase the worker produced and give it to a fresh agent, with the judge file as its prompt. A judge sees the codebase and its prompt, not the worker's conversation.
4. **Score.** Every LLM judge ends its reply with a line that is exactly `PASS` or `FAIL`, after a short paragraph of reasons; `judge.sh` prints `PASS` or `FAIL` per judge. The task passes when all its judges pass.

Metrics worth recording per run: pass/fail per judge, the worker's turns, tool calls, `learn` calls, failed transcript stanzas, and total tokens (the server instructions and every `learn` reply count).

Run each variant several times, since a single run is noise. Keep some tasks out of the loop (don't tune topics against them) to check for overfitting.

## Deterministic judges

A `*.deterministic.md` judge is a [transcript](learn/ucm/transcript.md) that `judge.sh` runs on a copy of the worker's codebase. It passes when the transcript completes, so a command that finds nothing (`ls Foo.tests` on missing tests) or code that doesn't typecheck (a call with the arguments in the wrong order) fails it, and when nothing in the output is marked 🚫 or FAILED, so a `test>` line asserts. Absence can't be asserted by a command, so a line `<!-- forbid: <regular expression> -->` fails the judge if any line of the output matches. Anchor those to the shape of the output you mean (the `dependencies` listing is `^ +[0-9]+\. .*Name$`), since the output repeats the judge's own prose.

## Writing an LLM judge

- Say exactly what to check and what to run.
- Name the topics the judge may read, if any. A judge that tests whether the documentation stands on its own must not read the topics that teach how to write it.
- End with: "Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL."

## Tasks

- `list-pick`: write a small documented, tested function. LLM judges for the documentation and the tests.
- `list-xf`: `List.xf`, a function that is `List.map` under another name. Don't read the source of `List` functions. Judges: tests exist and pass, no `List.reverse` or `List.+:` among its dependencies (tail recursive), called with the function first, and works on a million elements; and whether it is `List.map` (the worker looked before implementing).
- `list-sift`: the same for `List.sift`, which is `List.filter`.
- `list-xf-keep`: the same for `List.xfKeep`, which is `List.filterMap`.

The dependency check sees only the direct dependencies of the function, so a helper defined as its own term hides what it calls. The worker's prompt doesn't ask for helpers, and local `go` functions are fine.
