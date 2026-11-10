In the current Unison project, I need `List.sift : (a ->{g} Boolean) -> [a] ->{g} [a]`, which keeps the elements of a list for which a function returns true, in their original order. For example, `List.sift (x -> x > 1) [1, 2, 3]` is `[2, 3]`.

It should work on lists of any length. Add it to the codebase, with tests.

Don't look at the source code of any existing `List` function (`view`, `source` or similar on a definition in `List` or in a library). Reading documentation (`docs`) is fine.
