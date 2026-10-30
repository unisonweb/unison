``` unison
factorialDigitSum : Nat -> Nat
factorialDigitSum n =
  fact = List.foldLeft (Nat.*) 1 (List.range 1 (n + 1))
  digits = Text.toCharList (Nat.toText fact)
  List.foldLeft (acc c -> acc + (Char.toNat c - Char.toNat ?0)) 0 digits

test> factorialDigitSum.tests.example = test.verify do
  ensureEqual (factorialDigitSum 10) 27
  ensureEqual (factorialDigitSum 0) 1
  ensureEqual (factorialDigitSum 1) 1
```
``` ucm
scratch/main> add
scratch/main> test
```
