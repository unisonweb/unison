In the current Unison project, I need `wordIds : [Text] -> [Nat]`, which gives each distinct word a number in order of first appearance, starting at `0`, and returns the number of each word in the input, in order. For example, `wordIds ["a", "b", "a", "c", "b"]` is `[0, 1, 0, 2, 1]`. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
