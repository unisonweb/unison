``` unison
factorialDigitSum : Nat -> Nat
factorialDigitSum n =
  one = Natural.fromNat 1
  fact = List.foldLeft (acc k -> (Natural.*) acc (Natural.fromNat k)) one (List.range 1 (n + 1))
  digits = Text.toCharList (Natural.toText fact)
  List.foldLeft (acc c -> acc + (Char.toNat c - Char.toNat ?0)) 0 digits

test> factorialDigitSum.tests.example = test.verify do
  ensureEqual (factorialDigitSum 10) 27
  ensureEqual (factorialDigitSum 0) 1
  ensureEqual (factorialDigitSum 1) 1
  ensureEqual (factorialDigitSum 25) 72
  ensureEqual (factorialDigitSum 100) 648
  ensureEqual (factorialDigitSum 1000) 10539
  ensureEqual (factorialDigitSum 2000) 23382
```
``` ucm
scratch/main> add
scratch/main> test
```
