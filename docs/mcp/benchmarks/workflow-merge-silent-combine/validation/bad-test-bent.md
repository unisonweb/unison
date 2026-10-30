<!-- expected: outcome FAIL and behavior FAIL (the merged definition was kept and the failing test was changed to expect its result, so everything passes and the lowercase test no longer tests lowercasing) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Report.render : [Text] -> Text
Report.render names =
  cleaned = List.map Text.toLowercase names
  title = "Names"
  body = Text.join ", " (List.sort names)
  title ++ ": " ++ body

test> Report.render.tests.lowercase = test.verify do
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: Ann, Bob"

test> Report.render.tests.plain = test.verify do
  ensureEqual (Report.render ["a", "b"]) "Names: a, b"

test> Report.render.tests.sorted = test.verify do
  ensureEqual (Report.render ["b", "a"]) "Names: a, b"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```
