``` unison
List.mergeSorted : [a] -> [a] -> [a]
List.mergeSorted xs ys =
  go acc l r = match (l, r) with
    ([], _) -> acc ++ r
    (_, []) -> acc ++ l
    (a +: l', b +: r') ->
      if Universal.lteq a b then go (acc :+ a) l' r else go (acc :+ b) l r'
  go [] xs ys

test> List.mergeSorted.tests.example = test.verify do
  ensureEqual (List.mergeSorted [1, 4, 6] [2, 3, 7, 8]) [1, 2, 3, 4, 6, 7, 8]
  ensureEqual (List.mergeSorted [1, 2, 2] [2, 3]) [1, 2, 2, 2, 3]
  ensureEqual (List.mergeSorted [] [1, 2]) [1, 2]
  ensureEqual (List.mergeSorted [1, 2] []) [1, 2]
  ensureEqual (List.mergeSorted ([] : [Nat]) []) []
  ensureEqual (List.mergeSorted ["a", "c"] ["b"]) ["a", "b", "c"]
  ensureEqual (List.size (List.mergeSorted (List.range 0 20000) (List.range 10000 30000))) 40000
  ensureEqual (List.take 5 (List.mergeSorted (List.range 0 20000) (List.range 0 20000))) [0, 0, 1, 1, 2]
  ensureEqual (List.last (List.mergeSorted (List.range 0 20000) (List.range 0 20000))) (Some 19999)
```
``` ucm
scratch/main> add
scratch/main> test
```
