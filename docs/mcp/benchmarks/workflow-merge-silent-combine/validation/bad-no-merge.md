<!-- expected: approach PASS (outcome and behavior PASS, but bob was never merged: the combined definition was typed on alice) -->
``` unison
Report.render : [Text] -> Text
Report.render names =
  body = Text.join ", " (List.sort (List.map Text.toLowercase names))
  title = "Names"
  title ++ ": " ++ body

test> Report.render.tests.lowercase = test.verify do
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: ann, bob"

test> Report.render.tests.plain = test.verify do
  ensureEqual (Report.render ["a", "b"]) "Names: a, b"

test> Report.render.tests.sorted = test.verify do
  ensureEqual (Report.render ["b", "a"]) "Names: a, b"
```

``` ucm
scratch/alice> update
scratch/alice> test
```
