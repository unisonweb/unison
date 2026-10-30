The libraries `textkit` and `tally` stand in for Share libraries: local projects forked into `lib`, so this setup needs no network. `textkit` 2.0.0 only adds a function; `tally` 2.0.0 changes the type of `tick`.

``` ucm
scratch/main> project.create-empty textkit
textkit/main> builtins.merge lib.builtins
textkit/main> branch v1
```

``` unison
shout : Text -> Text
shout t = t ++ "!"

migration : Text
migration = "1.0.0 : shout."
```

``` ucm
textkit/v1> add
textkit/v1> branch /v2
```

``` unison
shout : Text -> Text
shout t = t ++ "!"

whisper : Text -> Text
whisper t = "(" ++ t ++ ")"

migration : Text
migration = "2.0.0 : adds whisper. Nothing else changed."
```

``` ucm
textkit/v2> update
scratch/main> project.create-empty tally
tally/main> builtins.merge lib.builtins
tally/main> branch v1
```

``` unison
unique type Tally = Tally Nat

empty : Tally
empty = Tally 0

tick : Tally -> Tally
tick = cases Tally n -> Tally (n + 1)

total : Tally -> Nat
total = cases Tally n -> n

migration : Text
migration = "1.0.0 : Tally, empty, tick, total."
```

``` ucm
tally/v1> add
tally/v1> branch /v2
```

``` unison
unique type Tally = Tally Nat

empty : Tally
empty = Tally 0

tick : Nat -> Tally -> Tally
tick amount = cases Tally n -> Tally (n + amount)

total : Tally -> Nat
total = cases Tally n -> n

migration : Text
migration = "2.0.0 : tick now takes the amount to add first, so tick t of 1.0.0 is tick 1 t now."
```

``` ucm
tally/v2> update
scratch/main> fork textkit/v1: lib.textkit_1_0_0
scratch/main> fork tally/v1: lib.tally_1_0_0
```

The project's own code, written against the 1.0.0 releases.

``` unison
excite : Text -> Text
excite name = shout name

countThree : Nat
countThree = total (tick (tick (tick empty)))

report : Text -> Text
report name = shout name ++ " " ++ Nat.toText countThree

excite.tests.hi = test.verify do ensureEqual (excite "hi") "hi!"
countThree.tests.three = test.verify do ensureEqual countThree 3
report.tests.go = test.verify do ensureEqual (report "go") "go! 3"
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> fork textkit/v2: lib.textkit_2_0_0
scratch/main> fork tally/v2: lib.tally_2_0_0
scratch/main> list lib
```
