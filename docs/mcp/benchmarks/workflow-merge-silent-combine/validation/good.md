<!-- The worker reads the file the merge wrote (merge-bob-into-alice-<hash>.u: no conflict markers, because the two edits are on different lines), notices that the merged render sorts the original names and lowercases the joined text afterwards, so the order is wrong for mixed case ("B", "a", "C" gives "b, c, a"), fixes it and loads the file. The lowercase test now expects the sorted order. Here the fixed definitions are written as a named file (resolved.u) and loaded, which is the same thing. -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
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
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```
