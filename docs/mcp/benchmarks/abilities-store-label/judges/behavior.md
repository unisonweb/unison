You are judging `Tree.label` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Tree` and `view Tree.label`: `Tree` has constructors `Tree.Leaf` and `Tree.Branch`, and `Tree.label : Tree a -> Tree (Nat, a)` (pure: no abilities left in the type).
2. `Tree.label (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))` is `Branch (Leaf (0, "a")) (Branch (Leaf (1, "b")) (Leaf (2, "c")))`.
3. `Tree.label (Leaf "x")` is `Leaf (0, "x")`.
4. For a balanced tree of four leaves `((a b) (c d))`, the numbers are `0, 1, 2, 3` in left-to-right order.

`Tree.label` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
