<!-- expected: outcome FAIL and behavior FAIL (the merged definition was kept as it came and the failing lowercase test was changed to the order the wrong code produces, so all three tests pass, but the report is wrong for mixed case: "B", "a", "C" gives "b, c, a") -->
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
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: ann, bob"

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
