In the current Unison project, I need `List.runLengths : [a] -> [(a, Nat)]`, which collapses each run of equal adjacent elements into the element and the length of the run. For example, `List.runLengths ["a", "a", "a", "b", "c", "c", "a", "a"]` is `[("a", 3), ("b", 1), ("c", 2), ("a", 2)]`. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
