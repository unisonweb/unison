``` unison
average : [Int] ->{Exception} Int
average xs =
  if List.isEmpty xs then Exception.raiseGeneric "average of an empty list" xs
  else List.foldLeft (Int.+) +0 xs / Nat.toInt (List.size xs)

averageOrNone : [Int] -> Optional Int
averageOrNone xs = Either.toOptional (Exception.catch do average xs)

test> average.tests.example = test.verify do
  ensureEqual (Exception.catch do average [+2, +4, +6]) (Right +4)
  ensureEqual (Exception.catch do average [+7]) (Right +7)
  ensureEqual (averageOrNone [+3, +4]) (Some +3)
  ensureEqual (averageOrNone []) None
  ensure (Either.isLeft (Exception.catch do average []))
```
``` ucm
scratch/main> add
scratch/main> test
```
