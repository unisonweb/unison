You are judging the behavior of the project's functions in the Unison codebase you are connected to. A previous agent was asked to upgrade the dependencies `textkit` and `tally` to 2.0.0 (folders `lib.textkit_2_0_0` and `lib.tally_2_0_0`). The prompt did not mention it, but an upgrade must not change what the project's functions do: the behavior has to be exactly the same as before. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks. This check needs no network.

Evaluate each of the following (for example with `> report "go"` in a `unison` block, or `run`), and compare with the expected value, which comes from the 1.0.0 behavior:

1. `excite "hi"` is `"hi!"`, and `excite ""` is `"!"`.
2. `countThree` is `3`.
3. `heading "top"` is `"top!"`, and `heading ""` is `"!"`.
4. `report "go"` is `"go! 3"`, and `report "a b"` is `"a b! 3"`.
5. With the new library, `lib.tally_2_0_0.total (lib.tally_2_0_0.tick 1 (lib.tally_2_0_0.named "x"))` is `1` (so a count of one tick is an amount of 1, not of some other number).
6. `view excite countThree heading report`: the functions keep the types `Text -> Text`, `Nat`, `Text -> Text` and `Text -> Text`, and `countThree` is computed with the library's `tick` three times (an amount of 1 each), not written as the literal `3`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
