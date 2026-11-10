In the current Unison project, I need `List.runningTotals : [Nat] -> [Nat]`, where element `i` of the result is the sum of the first `i + 1` elements. For example, `List.runningTotals [1, 2, 3, 4]` is `[1, 3, 6, 10]`. The result has the same length as the input. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
