In the current Unison project, I need `groupByLength : [Text] -> Map Nat [Text]`, which groups words by their length. Within a group the words keep the order they had in the input. For example, `groupByLength ["a", "bb", "c", "dd", "eee"]` is `{1 -> ["a", "c"], 2 -> ["bb", "dd"], 3 -> ["eee"]}`. Add it to the codebase, with tests.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
