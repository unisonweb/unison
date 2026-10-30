You are judging `parseNats` in the Unison codebase you are connected to. A previous agent wrote it, using a JSON library installed from Share, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of the following (for example with `> parseNats "[1, 2, 3]"` in a `unison` block, or `run`):

1. `view parseNats`: its type is `Text -> Either Failure [Nat]` (the `Failure` is base's `IO.Failure`), and it has no abilities in its type.
2. `parseNats "[1, 2, 3]"` is `Right [1, 2, 3]`, `parseNats "[]"` is `Right []`, and `parseNats "[ 10 , 20 ]"` is `Right [10, 20]`. A value larger than a small number works too: `parseNats "[1000000, 7]"` is `Right [1000000, 7]`.
3. These are all a `Left` (none of them crashes the evaluation): `parseNats "[\"a\"]"`, `parseNats "[1, 2"` (truncated), `parseNats "{}"`, `parseNats "[-1]"`, `parseNats "[1.5]"`, `parseNats ""`.
4. `parseNats "[1, 2, 3]"` also works the second time (the same evaluation in a fresh block), so nothing depends on hidden state.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
