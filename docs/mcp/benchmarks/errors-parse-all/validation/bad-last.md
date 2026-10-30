``` unison
parseAll : [Text] -> Either Text [Nat]
parseAll xs =
  go acc bad = cases
    [] -> match bad with
      Some t -> Left t
      None -> Right acc
    t +: rest -> match Nat.fromText t with
      None -> go acc (Some t) rest
      Some n -> go (acc :+ n) bad rest
  go [] None xs

test> parseAll.tests.example = test.verify do
  ensureEqual (parseAll ["1", "2", "3"]) (Right [1, 2, 3])
  ensureEqual (parseAll []) (Right [])
  ensureEqual (parseAll ["-1"]) (Left "-1")
  ensureEqual (parseAll ["12a"]) (Left "12a")
  ensureEqual (parseAll ["1", ""]) (Left "")
  ensureEqual (parseAll (List.map (_ -> "5") (List.range 0 100))) (Right (List.map (_ -> 5) (List.range 0 100)))
```
``` ucm
scratch/main> add
scratch/main> test
```
