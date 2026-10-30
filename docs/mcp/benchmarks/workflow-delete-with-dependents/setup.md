A small paging library with a hardcoded page size, `Page.size`, that two functions use directly (`Page.first`, `Page.count`), one function that uses those two (`Page.summary`), and one on top of that (`Page.report`), each with a test with written-out values.

```` unison
Page.size : Nat
Page.size = 20

Page.first : [a] -> [a]
Page.first xs = List.take Page.size xs

Page.count : [a] -> Nat
Page.count xs =
  n = List.size xs
  pages = n / Page.size
  if Nat.mod n Page.size == 0 then pages else pages + 1

Page.summary : [a] -> Text
Page.summary xs =
  Nat.toText (Page.count xs) ++ " pages, first has " ++ Nat.toText (List.size (Page.first xs))

Page.report : [Text] -> Text
Page.report names = "report: " ++ Page.summary names

test> Page.first.tests.example = test.verify do
  ensureEqual (List.size (Page.first (List.range 0 45))) 20
  ensureEqual (Page.first [1, 2, 3]) [1, 2, 3]

test> Page.count.tests.example = test.verify do
  ensureEqual (Page.count (List.range 0 45)) 3
  ensureEqual (Page.count (List.range 0 40)) 2
  ensureEqual (Page.count (List.range 0 41)) 3
  ensureEqual (Page.count ([] : [Nat])) 0

test> Page.summary.tests.example = test.verify do
  ensureEqual (Page.summary (List.range 0 45)) "3 pages, first has 20"

test> Page.report.tests.example = test.verify do
  ensureEqual (Page.report (List.map Nat.toText (List.range 0 45))) "report: 3 pages, first has 20"
````

``` ucm
scratch/main> add
scratch/main> test
```
