You are judging the implementation of `List.sift` in the Unison codebase you are connected to. A previous agent implemented it and wrote tests. You may use any tool except `learn`. Don't change the codebase's definitions; use scratch code only for your own checks.

Check each of the following, and note the result of each:

1. **Tests exist.** `ls List.sift.tests` lists at least one test. (Tests for `foo` are named `foo.tests.<name>`.)
2. **Tests pass.** `test List.sift.tests` runs them and they all pass.
3. **Tail recursive.** Run `dependencies List.sift`. The list must not include `List.+:` or `List.reverse`. Building the result by prepending and then reversing is not what we want: the result should be built in order, by appending. If the implementation calls a helper function that is its own definition in the project, run `dependencies` on that helper as well.
4. **Argument order.** In a scratch file, evaluate `List.sift (x -> x > 1) [1, 2, 3]`. The result must be `[2, 3]`. The function comes first, then the list.
5. **Larger lists.** Evaluate `List.size (List.sift (x -> Nat.isEven x) (List.range 0 100))`. The result must be `50`.

The implementation passes only if all five checks pass.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
