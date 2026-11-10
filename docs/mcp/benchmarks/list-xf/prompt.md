In the current Unison project, I need `List.xf : (a ->{g} b) -> [a] ->{g} [b]`, which applies a function to every element of a list, and returns the list of results in the same order. For example, `List.xf (x -> x * 2) [1, 2, 3]` is `[2, 4, 6]`.

It should work on lists of any length. Add it to the codebase, with tests.

Don't look at the source code of any existing `List` function (`view`, `source` or similar on a definition in `List` or in a library). Reading documentation (`docs`) is fine.
