The worker's best case: first rewrite everything that uses `Page.size`, in one block, and save it with `update`; then nothing uses `Page.size` and `delete` removes it at once, with no temporary branch.

```` unison
Page.first : [a] -> Nat -> [a]
Page.first xs size = List.take size xs

Page.count : [a] -> Nat -> Nat
Page.count xs size =
  n = List.size xs
  pages = n / size
  if Nat.mod n size == 0 then pages else pages + 1

Page.summary : [a] -> Nat -> Text
Page.summary xs size =
  Nat.toText (Page.count xs size) ++ " pages, first has " ++ Nat.toText (List.size (Page.first xs size))

Page.report : [Text] -> Nat -> Text
Page.report names size = "report: " ++ Page.summary names size

test> Page.first.tests.example = test.verify do
  ensureEqual (List.size (Page.first (List.range 0 45) 20)) 20
  ensureEqual (List.size (Page.first (List.range 0 45) 10)) 10
  ensureEqual (Page.first [1, 2, 3] 20) [1, 2, 3]

test> Page.count.tests.example = test.verify do
  ensureEqual (Page.count (List.range 0 45) 20) 3
  ensureEqual (Page.count (List.range 0 40) 20) 2
  ensureEqual (Page.count (List.range 0 41) 20) 3
  ensureEqual (Page.count (List.range 0 45) 10) 5
  ensureEqual (Page.count ([] : [Nat]) 20) 0

test> Page.summary.tests.example = test.verify do
  ensureEqual (Page.summary (List.range 0 45) 20) "3 pages, first has 20"
  ensureEqual (Page.summary (List.range 0 45) 10) "5 pages, first has 10"

test> Page.report.tests.example = test.verify do
  ensureEqual (Page.report (List.map Nat.toText (List.range 0 45)) 20) "report: 3 pages, first has 20"
````

``` ucm
scratch/main> update
scratch/main> delete.term Page.size
scratch/main> test
scratch/main> todo
scratch/main> branches
```
