<!-- expected: approach FAIL (outcome and no-collateral PASS; a retyped copy has the same hashes but no merge in the history) -->
``` unison
Temperature.toKelvin : Nat -> Nat
Temperature.toKelvin c = c + 273

test> Temperature.toKelvin.tests.example = test.verify do
  ensureEqual (Temperature.toKelvin 100) 373
  ensureEqual (Temperature.toKelvin 0) 273
```
``` ucm
scratch/alice> add
scratch/alice> test
```
