You are judging `sampleSeeded` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view sampleSeeded`: the type is `Nat -> Nat -> [a] -> [a]`, with no abilities.
2. `sampleSeeded 7 2 [10, 20, 30, 40]` evaluated twice gives the same list, of size 2, whose two elements are different and both in `[10, 20, 30, 40]`.
3. `List.sort (sampleSeeded 3 10 [1, 2, 3])` is `[1, 2, 3]`, and `List.sort (sampleSeeded 3 3 [1, 2, 3])` is `[1, 2, 3]`.
4. `sampleSeeded 1 0 [1, 2, 3]` is `[]`, and `sampleSeeded 1 2 ([] : [Nat])` is `[]`.
5. For `xs = List.range 0 50`: `List.size (sampleSeeded 9 5 xs)` is `5`, and `List.size (List.distinct (sampleSeeded 9 5 xs))` is `5`.
6. Over the seeds `0` to `9`, `sampleSeeded seed 3 (List.range 0 20)` gives at least two different results (sorted).

`sampleSeeded` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
