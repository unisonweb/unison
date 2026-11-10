<!-- expected: outcome FAIL and behavior FAIL (the merged file was loaded without reading it: render sorts the names and lowercases the joined text, and the lowercase test, still expecting the old order, fails) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Report.render : [Text] -> Text
Report.render names =
  body = Text.join ", " (List.sort names)
  title = "Names"
  title ++ ": " ++ Text.toLowercase body

test> Report.render.tests.lowercase = test.verify do
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: bob, ann"

test> Report.render.tests.plain = test.verify do
  ensureEqual (Report.render ["a", "b"]) "Names: a, b"

test> Report.render.tests.sorted = test.verify do
  ensureEqual (Report.render ["b", "a"]) "Names: a, b"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
```

``` ucm :error
scratch/alice> test
```
