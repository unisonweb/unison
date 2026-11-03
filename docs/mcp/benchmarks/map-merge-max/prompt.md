In the current Unison project, I need `Map.mergeMax : Map Text Nat -> Map Text Nat -> Map Text Nat`, which combines two maps of scores: a key in both maps gets the larger of its two values, and a key in only one keeps its value. For example, merging `Map.fromList [("a", 3), ("b", 9)]` with `Map.fromList [("b", 4), ("c", 1)]` gives a map with `a -> 3`, `b -> 9` and `c -> 1`. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
