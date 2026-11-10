``` unison
wordCounts : Text -> Map Text Nat
wordCounts t = Map.fromListWith (Nat.+) (List.map (w -> (w, 1)) (Text.words (Text.toLowercase t)))

test> wordCounts.tests.example = test.verify do
  ensureEqual (Map.toList (wordCounts "The cat and the hat")) [("and", 1), ("cat", 1), ("hat", 1), ("the", 2)]
  ensureEqual (Map.toList (wordCounts "  a\tA\n\nb   a ")) [("a", 3), ("b", 1)]
  ensureEqual (Map.toList (wordCounts "")) []
```
``` ucm
scratch/main> add
scratch/main> test
```
