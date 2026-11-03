In the current Unison project, I need `List.neighbors : [a] -> [(a, a)]`, which pairs each element with the one after it. For example, `List.neighbors [1, 2, 3, 4]` is `[(1, 2), (2, 3), (3, 4)]`. A list with fewer than two elements has no pairs. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
