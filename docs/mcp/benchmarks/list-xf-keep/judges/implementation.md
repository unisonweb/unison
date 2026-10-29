You are judging the implementation of `List.xfKeep` in the Unison codebase you are connected to. A previous agent implemented it and wrote tests. You may use any tool except `learn`. Don't change the codebase's definitions; use scratch code only for your own checks.

Check each of the following, and note the result of each:

1. **Tests exist.** `ls List.xfKeep.tests` lists at least one test. (Tests for `foo` are named `foo.tests.<name>`.)
2. **Tests pass.** `test List.xfKeep.tests` runs them and they all pass.
3. **Tail recursive.** Run `dependencies List.xfKeep`. The list must not include `List.+:` or `List.reverse`. Building the result by prepending and then reversing is not what we want: the result should be built in order, by appending. If the implementation calls a helper function that is its own definition in the project, run `dependencies` on that helper as well.
4. **Argument order.** In a scratch file, evaluate `List.xfKeep (x -> if x > 1 then Some (x * 10) else None) [1, 2, 3]`. The result must be `[20, 30]`. The function comes first, then the list.
5. **Long lists.** Evaluate `List.size (List.xfKeep (x -> if Nat.isEven x then Some x else None) (List.range 0 1000000))`. The result must be `500000` and it must finish quickly, without a stack overflow.

The implementation passes only if all five checks pass.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
