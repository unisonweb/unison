You are judging the tests of `List.pick` in the Unison codebase you are connected to. A previous agent wrote `List.pick` and tests for it.

You may use any tool, but not the `learn` tool.

Steps:

1. Find the tests for `List.pick` (they are in the project, not in a library) and view them.
2. Run them. They must all pass.
3. Check that they cover: an ordinary case with both true and false flags, the empty list, all flags false, all flags true, and lists of different lengths (decide what the function does, and check that the tests and the documentation agree).
4. Check that the expected values are written out in the tests, not computed by calling `List.pick` itself.
5. Check that the definitions were added to the codebase, not only left in a scratch file.

The tests pass if they run and pass, cover those cases, and state their expectations independently of the implementation. Otherwise they fail.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
