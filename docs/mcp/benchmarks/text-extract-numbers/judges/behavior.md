You are judging `Text.numbers` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Text.numbers`: the type is `Text -> [Int]`.
2. `Text.numbers "a1 b-22, c333"` is `[+1, -22, +333]`.
3. `Text.numbers "x12y 7"` is `[+12, +7]`; digits inside words count.
4. `Text.numbers "no digits here"` and `Text.numbers ""` are `[]`.
5. `Text.numbers "-5"` is `[-5]` and `Text.numbers "3-4"` is `[+3, -4]`.

`Text.numbers` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
