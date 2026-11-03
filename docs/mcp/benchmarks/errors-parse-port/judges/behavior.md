You are judging `Port.parse` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Port.parse` and `view Port.parseOrNone`: the types are `Text ->{Exception} Nat` and `Text -> Optional Nat`.
2. `catch do Port.parse "8080"` is `Right 8080`, and `catch do Port.parse "1"` and `catch do Port.parse "65535"` are `Right 1` and `Right 65535`.
3. `catch do Port.parse "0"`, `catch do Port.parse "65536"`, `catch do Port.parse "http"`, `catch do Port.parse ""` and `catch do Port.parse "80abc"` are all `Left` (check with `Either.isLeft`).
4. `Port.parseOrNone "443"` is `Some 443`, and `Port.parseOrNone "http"`, `Port.parseOrNone "0"` and `Port.parseOrNone "-5"` are `None`.

`Port.parse` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
