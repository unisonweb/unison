``` unison
Temperature.boiling : Int
Temperature.boiling = +100

test> Temperature.boiling.tests.example = test.verify do
  ensureEqual Temperature.boiling +100
```

``` ucm
scratch/main> add
scratch/main> branch alice
scratch/main> branch bob
```

``` unison
Temperature.toFahrenheit : Int -> Int
Temperature.toFahrenheit c = c * +9 / +5 + +32

test> Temperature.toFahrenheit.tests.example = test.verify do
  ensureEqual (Temperature.toFahrenheit +100) +212
  ensureEqual (Temperature.toFahrenheit +0) +32
```

``` ucm
scratch/alice> add
```

``` ucm
scratch/bob> ls Temperature
```

``` unison
Temperature.toKelvin : Int -> Int
Temperature.toKelvin c = c + +273

test> Temperature.toKelvin.tests.example = test.verify do
  ensureEqual (Temperature.toKelvin +100) +373
  ensureEqual (Temperature.toKelvin +0) +273
```

``` ucm
scratch/bob> add
scratch/alice> switch /alice
```
