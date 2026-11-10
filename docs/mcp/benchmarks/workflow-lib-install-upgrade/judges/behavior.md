You are judging `parseNats` in the Unison codebase you are connected to. A previous agent moved the project to the latest release of the JSON library `@unison/json` (a folder `lib.unison_json_<version>` with a version later than 1.2.1) and was asked to keep `parseNats` working. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of the following (for example with `> parseNats "[1, 2, 3]"` in a `unison` block, or `run`):

1. `parseNats "[1, 2, 3]"` is `Right [1, 2, 3]`, and `parseNats "[]"` is `Right []`, and `parseNats "[ 10 , 20 ]"` is `Right [10, 20]`.
2. `parseNats "[\"a\"]"` is a `Left`, and so are `parseNats "[1, 2"` (truncated), `parseNats "{}"` and `parseNats "[-1]"`. None of them crashes the evaluation.
3. `view parseNats`: its type is still `Text -> Either Failure [Nat]` and it still decodes through the library (`Decoder.run`, `Decoder.array`, `Decoder.nat`, or the equivalents of the new release). It must not be a hand-written parser, and must not be a lookup table of the inputs above.
4. `docs parseNats` still shows its documentation.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
