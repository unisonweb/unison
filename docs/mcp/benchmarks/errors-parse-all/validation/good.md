``` unison
parseAll : [Text] -> Either Text [Nat]
parseAll xs = Throw.toEither do List.map (t -> Optional.orThrow t (Nat.fromText t)) xs

test> parseAll.tests.example = test.verify do
  ensureEqual (parseAll ["1", "2", "3"]) (Right [1, 2, 3])
  ensureEqual (parseAll ["1", "x", "y"]) (Left "x")
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
