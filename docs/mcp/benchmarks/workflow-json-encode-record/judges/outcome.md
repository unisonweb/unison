You are judging the work on `Person.toJson` and `Person.fromJson` in the Unison codebase you are connected to. The project `scratch` had the record type `Person` (`name : Text`, `age : Nat`) and `@unison/base`; a previous agent had to add `Person.toJson : Person -> Text` and `Person.fromJson : Text ->{Exception} Person` (it raises when the text is not valid JSON of the right shape); the prompt did not ask for tests, but a good agent adds them. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks. Don't depend on Unison Share being reachable.

Run each of the following and note the result of each:

1. `view Person.toJson` and `view Person.fromJson`: the types are exactly `Person -> Text` and `Text ->{Exception} Person` (`toJson` has no abilities; `fromJson` has `Exception` and nothing else).
2. `view Person` still shows the record with the fields `name : Text` and `age : Nat`.
3. `list lib` shows a JSON library installed next to `@unison/base` (a folder like `unison_json_...`), so the functions use a library and are not hand-built. (Which library is judged elsewhere; here it only has to be there and `view Person.toJson` must reach into it, which `dependencies Person.toJson` and `dependencies Person.fromJson` confirm.)
4. `test` runs every test in the project without a failure, and there is at least one test of each function in the project namespace, not in a library (`find tests`, `ls Person`, `view` them). The tests cover a round trip (`fromJson (toJson p) == p`) and at least one failure case (invalid JSON text, a missing field, or a field of the wrong type) checked by catching the exception (`catch`, `toEither`). The expected values are written out, not computed with the functions under test.
5. The definitions and tests are in the codebase (found by `view`), not only in a scratch file.
6. `branches` shows the project on `main`; there is no leftover temporary branch. `todo` reports nothing pending.
7. `docs Person.toJson` and `docs Person.fromJson` are not required; do not fail on missing documentation.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
