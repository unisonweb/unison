In the current Unison project, I need `wordCounts : Text -> Map Text Nat`, which counts how many times each word occurs in a text. Words are separated by whitespace, and case is ignored (the keys are lowercase). For example, `wordCounts "The cat and the hat"` is a map with `"the"` -> 2 and `"cat"`, `"and"`, `"hat"` -> 1. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
