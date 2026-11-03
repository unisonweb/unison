You are judging the tests of `shuffleSeeded` in the Unison codebase you are connected to. A previous agent wrote `shuffleSeeded` and tests for it. You may use any tool except `learn`.

Steps:

1. Find the tests for `shuffleSeeded` (`ls shuffleSeeded.tests`, or `find tests`; they are in the project, not in a library) and view them.
2. Run them with `test`. They must all pass.
3. Check that they cover: determinism (the same seed twice), that the result is a permutation, and the empty list.
4. Check that the expected values are written out in the tests, not computed by calling `shuffleSeeded` itself.
5. Check that the definitions and the tests were added to the codebase, not only left in a scratch file.

The tests pass if they run and pass, cover those cases, and state their expectations independently of the implementation. Otherwise they fail.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
