# MCP server and UCM: todo, for later

## `share.search`: numbered results, better layout, and a new `share.docs`

(From Paul, 2026-10-30. Not started.)

Today `share.search take Nat -> [a] -> [a]` prints, for each match, the full reference on one line (`@unison/base/releases/7.20.0:data.List.take`) and the type indented under it. The project and release are in front of the name, which makes the list hard to scan. Changes:

1. **Return a numbered list and fill in the numbered args**, so that later commands can refer to a result by number (`share.source 2`, `share.docs 2`, `share.render 2`), as `share.releases` does (see `SA.ShareReleaseRef`; a result needs its own structured argument, carrying the full `@user/project/releases/x.y.z:name` reference).
2. **New layout.** Show the project-local qualified name and the type first, then below it, indented and in gray, where the result comes from:

   ```
   1. builtin data.List.take : Nat -> [a] -> [a]
        (in @unison/base/releases/7.20.0)
   ```

3. **A tip after the results** that says how to see the source code or the docs of any result, either by number or by explicit path. For the explicit path, use the first result of the list as the example (for example `share.source @unison/base/releases/7.20.0:data.List.take`).
4. **New command `share.docs`**: shows the function signature (or type header), then the rendered docs of that definition, like the normal `docs` command does. It takes a number from the last `share.search` or an explicit `@user/project[/releases/x.y.z]:name`. (Today `share.render @user/project:name` prints only the rendered docs, with no signature.)

Also: add `share.docs` to the read-only tier in `unison-cli/src/Unison/MCP/Allowlist.hs`, document the commands in `docs/mcp/learn/ucm/share.md` and `ucm/libraries.md` (finding a library, then its docs), and add a transcript test (network).

## Clean up after the benchmark rounds

When the benchmark work is finished, delete the harness data (frozen UCM binary, cached base codebases, one directory per run, about several GB). The harness scripts themselves are in `docs/mcp/benchmarks/harness/` and stay.

```
rm -rf ~/.cache/unison-mcp-bench
```

(Check first that no round is still running: `pgrep -f unison-mcp-bench`.) Claude: remind Paul of this in the final report of the benchmark session, and give him the command.
