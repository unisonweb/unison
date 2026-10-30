You are judging the tests and documentation of `parseNats` in the Unison codebase you are connected to. A previous agent wrote `parseNats` (JSON text that is an array of non-negative whole numbers into `Either Failure [Nat]`), with tests and documentation. You may use any tool except `learn`. Don't change the project's definitions.

Steps:

1. Find the tests (`ls parseNats` and `find tests`; they are in the project, not in a library) and view them.
2. Run them with `test`. They must all pass.
3. Check that they cover an ordinary array (`"[1, 2, 3]"`), the empty array, and at least one failure case (not numbers, or not JSON), with the expected values written out and not computed by `parseNats` itself.
4. Check that the definitions and the tests are in the codebase, not only in a scratch file.
5. Run `docs parseNats`: it says what the function does, in more than its name, and agrees with how it behaves (a `Left` for invalid input).

It passes if the tests run, pass and cover those cases, and the documentation is present and sensible. Otherwise it fails.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
