You are judging `Counter.from` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Counter`: the ability has one operation, `next`, of type `() ->{Counter} Nat` (or `'{Counter} Nat`).
2. `view Counter.from`: it takes the starting `Nat` and a delayed computation, and returns the computation's result as a plain value.
3. `Counter.from 10 do (Counter.next(), Counter.next(), Counter.next())` is `(10, 11, 12)`.
4. `Counter.from 0 do List.map (_ -> Counter.next()) [1, 2, 3, 4]` is `[0, 1, 2, 3]` (the numbers go in list order).
5. `Counter.from 5 do "no counting"` is `"no counting"`.
6. Nesting: `Counter.from 0 do (Counter.next(), Counter.from 100 do Counter.next(), Counter.next())` is `(0, 100, 1)` (the inner handler has its own count).

`Counter.from` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
