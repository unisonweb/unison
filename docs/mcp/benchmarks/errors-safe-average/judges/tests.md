You are judging the tests of `average` in the Unison codebase you are connected to. A previous agent wrote `average` and tests for it. You may use any tool except `learn`.

Steps:

1. Find the tests for `average` (`ls average.tests`, or `find tests`; they are in the project, not in a library) and view them.
2. Run them with `test`. They must all pass.
3. Check that they cover: an ordinary list, a single element, and the empty list for both functions (the empty case of `average` should check that it fails, for example with `Exception.catch`; the tests for `averageOrNone` may be separate).
4. Check that the expected values are written out in the tests, not computed by calling `average` itself.
5. Check that the definitions and the tests were added to the codebase, not only left in a scratch file.

The tests pass if they run and pass, cover those cases, and state their expectations independently of the implementation. Otherwise they fail.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
