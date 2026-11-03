You are judging the behavior of `Cfg.parse`, `Cfg.render` and `Cfg.defaultPort` on branch `main` of the project `scratch` in the Unison codebase you are connected to. A previous agent restored `Cfg.parse` to an earlier version and was to keep later work. You may use any tool except `learn`. Don't change the project's definitions or branches.

The first version of `Cfg.parse` was `Nat.fromText t`. Run a transcript with the stanza `scratch/main> ls Cfg` followed by a `unison` block with watch expressions, such as `> Cfg.parse "8080"`, whose value is printed, and check:

1. `Cfg.parse "8080"` is `Some 8080`, `Cfg.parse "007"` is `Some 7`, `Cfg.parse "0"` is `Some 0`.
2. `Cfg.parse "abc"` is `None`, `Cfg.parse ""` is `None`, `Cfg.parse "-5"` is `None`, `Cfg.parse "1 2"` is `None`.
3. `Cfg.render "8080"` is `"port 8080"` and `Cfg.render "abc"` is `"invalid"` (so `render` follows the restored `parse`, not the wrong one that gave `port 0`).
4. `Cfg.defaultPort` is `8080`.

It passes if every check passes. If a definition cannot be found or the code doesn't typecheck, that is a FAIL.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
