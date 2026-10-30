<!-- The worker reads the file the merge wrote (merge-bob-into-alice-<hash>.u: no conflict markers, because the two edits are on different lines), notices that the combined render sorts `names` and ignores `cleaned`, fixes it and loads the file. Here the fixed definitions are written as a named file (resolved.u) and loaded, which is the same thing. -->
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
scratch/merge-bob-into-alice> update
scratch/alice> test
```
