In the current Unison project, I need `Map.invert : Map k v -> Map v [k]`, which groups the keys by value. For example, inverting the map `{1 -> "a", 2 -> "b", 3 -> "a"}` gives `{"a" -> [1, 3], "b" -> [2]}`. Within each list the keys are in ascending order. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
