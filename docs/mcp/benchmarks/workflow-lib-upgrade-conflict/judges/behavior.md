You are judging the behavior of the project's functions in the Unison codebase you are connected to. A previous agent was asked to upgrade the dependency `mylib` from 1.0.0 to 2.0.0 (folder `lib.mylib_2_0_0`) and moved the project's code over. The prompt did not mention it, but an upgrade must not change what the project's functions do: the behavior has to be exactly the same as before. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks. This check needs no network.

Evaluate each of the following (for example with `> welcome "bob"` in a `unison` block, or `run`), and compare with the expected value, which comes from the 1.0.0 behavior:

1. `welcome "bob"` is `"Hello, bob"`, and `welcome "al"` is `"Hello, al"`.
2. `shout 2 "bob"` is `"Hello, bob Hello, bob"`, `shout 3 "al"` is `"Hello, al Hello, al Hello, al"`, `shout 1 "x"` is `"Hello, x"`, and `shout 0 "x"` is `""`.
3. `describe twice` is `"repeat 2"`, and `describe (Opt 5 "-")` is `"repeat 5"` (`Opt` now has two fields; if the name is ambiguous write `lib.mylib_2_0_0.Opt.Opt`).
4. `twice` keeps its old meaning with the new library: `lib.mylib_2_0_0.render twice "bob"` is `"Hello, bob Hello, bob"` (two copies, one space between them).
5. `view welcome shout twice describe`: the functions keep the types `Text -> Text`, `Nat -> Text -> Text`, `Opt`, `Opt -> Text`, and the code is real code, not a lookup table for the inputs above.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
