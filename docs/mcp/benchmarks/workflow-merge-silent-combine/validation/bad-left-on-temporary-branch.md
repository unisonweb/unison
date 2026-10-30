<!-- expected: outcome FAIL (the file is fixed and loaded but update was never run, so it ends on merge-bob-into-alice) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Report.render : [Text] -> Text
Report.render names =
  cleaned = List.map Text.toLowercase names
  title = "Names"
  body = Text.join ", " (List.sort cleaned)
  title ++ ": " ++ body

test> Report.render.tests.lowercase = test.verify do
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: ann, bob"

test> Report.render.tests.plain = test.verify do
  ensureEqual (Report.render ["a", "b"]) "Names: a, b"

test> Report.render.tests.sorted = test.verify do
  ensureEqual (Report.render ["b", "a"]) "Names: a, b"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
```
