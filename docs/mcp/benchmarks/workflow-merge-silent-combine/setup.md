``` unison
Report.render : [Text] -> Text
Report.render names =
  cleaned = names
  title = "Names"
  body = Text.join ", " cleaned
  title ++ ": " ++ body

test> Report.render.tests.plain = test.verify do
  ensureEqual (Report.render ["a", "b"]) "Names: a, b"
```

``` ucm
scratch/main> add
scratch/main> branch alice
scratch/main> branch bob
```

Alice makes the names come out in lowercase.

``` ucm
scratch/alice> ls Report
```

``` unison
Report.render : [Text] -> Text
Report.render names =
  cleaned = List.map Text.toLowercase names
  title = "Names"
  body = Text.join ", " cleaned
  title ++ ": " ++ body

test> Report.render.tests.lowercase = test.verify do
  ensureEqual (Report.render ["Bob", "Ann"]) "Names: bob, ann"
```

``` ucm
scratch/alice> update
```

Bob makes the names come out sorted.

``` ucm
scratch/bob> ls Report
```

``` unison
Report.render : [Text] -> Text
Report.render names =
  cleaned = names
  title = "Names"
  body = Text.join ", " (List.sort names)
  title ++ ": " ++ body

test> Report.render.tests.sorted = test.verify do
  ensureEqual (Report.render ["b", "a"]) "Names: a, b"
```

``` ucm
scratch/bob> update
scratch/alice> switch /alice
```
