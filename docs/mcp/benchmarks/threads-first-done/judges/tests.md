You are judging the tests of `firstDone` in the Unison codebase you are connected to. A previous agent wrote `firstDone` and tests for it. You may use any tool except `learn`.

Steps:

1. Find the tests for `firstDone` (`ls firstDone.tests`, or `find tests`; they are in the project, not in a library) and view them.
2. Run them with `io.test` (it needs the IO transcript tool). They must all pass.
3. Check that they cover: the example in the task, a single task, and the empty list (which should fail with an `Exception`). An IO test is a definition starting with `do`, run with `io.test`.
4. Check that the expected values are written out in the tests, not computed by calling `firstDone` itself.
5. Check that the definitions and the tests were added to the codebase, not only left in a scratch file.

The tests pass if they run and pass, cover those cases, and state their expectations independently of the implementation. Otherwise they fail.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
