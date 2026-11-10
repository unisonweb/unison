``` unison
groupByLength : [Text] -> Map Nat [Text]
groupByLength ws = Map.map List.Nonempty.toList (List.groupMap Text.size (w -> w) ws)

test> groupByLength.tests.example = test.verify do
  ensureEqual (Map.toList (groupByLength ["a", "bb", "c", "dd", "eee"])) [(1, ["a", "c"]), (2, ["bb", "dd"]), (3, ["eee"])]
  ensureEqual (Map.get 2 (groupByLength ["zz", "aa", "mm"])) (Some ["zz", "aa", "mm"])
  ensureEqual (Map.get 1 (groupByLength ["a", "a"])) (Some ["a", "a"])
  ensureEqual (Map.toList (groupByLength [])) []
```
``` ucm
scratch/main> add
scratch/main> test
```
