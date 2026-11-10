You are judging `KV.parse` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view KV.parse`: the type is `Text -> Optional (Map Text Text)`.
2. `Optional.map Map.toList (KV.parse "host=example.com;port=8080")` is `Some [("host", "example.com"), ("port", "8080")]`.
3. `Optional.map Map.toList (KV.parse "a=;b=x y z")` is `Some [("a", ""), ("b", "x y z")]` (an empty value, and spaces inside a value).
4. `KV.parse ""` is `Some Map.empty` (check `Optional.map Map.size` gives `Some 0`).
5. `KV.parse "host"`, `KV.parse "a b=1"`, `KV.parse "a=1;"` and `KV.parse "=1"` are all `None`.
6. `Optional.map Map.toList (KV.parse "k=1;k=2")` is `Some [("k", "2")]` or `Some [("k", "1")]` (either is acceptable).

`KV.parse` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
