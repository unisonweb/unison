In the current Unison project, I need `Map.addCounts : Map k Nat -> Map k Nat -> Map k Nat`, which adds two maps of counts: a key in both gets the sum of its two values, and a key in only one keeps its value. For example, adding `{"a" -> 1, "b" -> 2}` and `{"b" -> 5, "c" -> 1}` gives `{"a" -> 1, "b" -> 7, "c" -> 1}`. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
