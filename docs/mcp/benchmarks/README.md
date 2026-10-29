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

## Protocol

1. **Prepare** a fresh codebase (the base library installed, then `setup.md` if present).
2. **Work.** A fresh agent with only the MCP server connected gets `prompt.md` and nothing else. It is not told about `learn` topics or the repository. Record its tool calls, the tokens it used, and its turns.
3. **Judge.** For each file in `judges/`, copy the codebase the worker produced and give it to a fresh agent, with the judge file as its prompt. A judge sees the codebase and its prompt, not the worker's conversation.
4. **Score.** Every judge ends its reply with a line that is exactly `PASS` or `FAIL`, after a short paragraph of reasons. The task passes when all its judges pass.

Metrics worth recording per run: pass/fail per judge, the worker's turns, tool calls, `learn` calls, failed transcript stanzas, and total tokens (the server instructions and every `learn` reply count).

Run each variant several times, since a single run is noise. Keep some tasks out of the loop (don't tune topics against them) to check for overfitting.

## Writing a judge

- Say exactly what to check and what to run.
- Name the topics the judge may read, if any. A judge that tests whether the documentation stands on its own must not read the topics that teach how to write it.
- End with: "Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL."

## Tasks

- `list-pick`: write a small documented, tested function.
