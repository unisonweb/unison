In the current Unison project, I need `List.xfKeep : (a ->{g} Optional b) -> [a] ->{g} [b]`, which applies a function to every element of a list, and keeps the results that are `Some`, dropping the `None`s, in their original order. For example, `List.xfKeep (x -> if x > 1 then Some (x * 10) else None) [1, 2, 3]` is `[20, 30]`.

It should work on lists of any length. Add it to the codebase, with tests.

Don't look at the source code of any existing `List` function (`view`, `source` or similar on a definition in `List` or in a library). Reading documentation (`docs`) is fine.
