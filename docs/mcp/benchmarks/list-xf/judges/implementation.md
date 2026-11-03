You are judging the implementation of `List.xf` in the Unison codebase you are connected to. A previous agent implemented it and wrote tests. You may use any tool except `learn`. Don't change the codebase's definitions; use scratch code only for your own checks.

Check each of the following, and note the result of each:

1. **Tests exist.** `ls List.xf.tests` lists at least one test. (Tests for `foo` are named `foo.tests.<name>`.)
2. **Tests pass.** `test List.xf.tests` runs them and they all pass.
3. **Tail recursive.** View the implementation and check that it is tail recursive: wherever it recurses, the recursive call is the last thing done, with the result built up in an accumulator, in order, by appending. Use `dependencies List.xf` to find the functions it calls, and view and check any helper that is its own definition in the project, following the chain as far as the project's own definitions go (library functions don't need checking). It fails if any recursion in the chain is not in tail position, or if it builds the result by prepending and then reversing (so `List.+:` or `List.reverse` among the dependencies is a sign of failure). Library functions such as folds that are themselves tail recursive are fine.
4. **Argument order.** In a scratch file, evaluate `List.xf (x -> x * 2) [1, 2, 3]`. The result must be `[2, 4, 6]`. The function comes first, then the list.
5. **Larger lists.** Evaluate `List.size (List.xf (x -> x + 1) (List.range 0 100))`. The result must be `100`.

The implementation passes only if all five checks pass.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
