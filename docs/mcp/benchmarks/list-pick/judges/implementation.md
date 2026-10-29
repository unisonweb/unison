You are judging the implementation of `List.pick` in the Unison codebase you are connected to. A previous agent implemented it, with documentation and tests. `List.pick flags xs` returns the elements of `xs` at the positions where `flags` is true: the values themselves, not their indices. You may use any tool except `learn`. Don't change the codebase's definitions; use scratch code only for your own checks.

Check each of the following, and note the result of each:

1. **Type.** `List.pick` has type `[Boolean] -> [a] -> [a]`.
2. **Values, not indices.** In a scratch file, evaluate `List.pick [false, true, true] [10, 20, 30]`. The result must be `[20, 30]` (a function that returned indices would give `[1, 2]`). Evaluate `List.pick [true, false, true] ["a", "b", "c"]`; the result must be `["a", "c"]`.
3. **Argument order.** The flags come first and the values second, as in the calls above.
4. **Edge cases.** `List.pick [] []` is `[]`, all-false flags give `[]`, and all-true flags give the whole list.
5. **Tail recursive.** Run `dependencies List.pick`. The list must not include `List.+:` or `List.reverse`: the result should be built in order, by appending, not by prepending and reversing. If the implementation calls a helper function that is its own definition in the project, run `dependencies` on that helper as well.
6. **Larger lists.** Evaluate `List.size (List.pick (List.map Nat.isEven (List.range 0 100)) (List.range 0 100))`. The result must be `50`.

The implementation passes only if all six checks pass.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
