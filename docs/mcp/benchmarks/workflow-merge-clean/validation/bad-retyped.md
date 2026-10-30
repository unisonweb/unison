<!-- expected: approach PASS (outcome and no-collateral PASS; a retyped copy has the same hashes) -->
``` unison
Temperature.toKelvin : Int -> Int
Temperature.toKelvin c = c + +273

test> Temperature.toKelvin.tests.example = test.verify do
  ensureEqual (Temperature.toKelvin +100) +373
  ensureEqual (Temperature.toKelvin +0) +273
```
``` ucm
scratch/alice> add
scratch/alice> test
```
