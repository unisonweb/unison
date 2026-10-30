The libraries `textkit` and `tally` stand in for Share libraries: local projects forked into `lib`, so this setup needs no network. `tally` depends on `textkit` (its `Tally` carries a `textkit` `Label`), as real libraries do: each `tally` release has its own copy of the `textkit` release it was built with in its own `lib`. `textkit` 2.0.0 changes `Label` (a second field) and `tally` 2.0.0 is built on `textkit` 2.0.0; it also changes the type of `tick`. The project mixes the two libraries in `heading`, which passes the `Label` that `tally` returns to `textkit`'s `shout`: with only one of the two libraries upgraded the two sides disagree about what a `Label` is.

``` ucm
scratch/main> project.create-empty textkit
textkit/main> builtins.merge lib.builtins
textkit/main> branch v1
```

``` unison
unique type Label = Label Text

label : Text -> Label
label t = Label t

shout : Label -> Text
shout = cases Label t -> t ++ "!"

migration : Text
migration = "1.0.0 : Label, label, shout."
```

``` ucm
textkit/v1> add
textkit/v1> branch /v2
```

``` unison
unique type Label = Label Text Nat

label : Text -> Label
label t = Label t 1

shout : Label -> Text
shout = cases Label t k -> t ++ Text.repeat k "!"

migration : Text
migration = "2.0.0 : Label has a second field, the number of exclamation marks, and label t of 1.0.0 is Label t 1 now. shout, label and the rest keep their names and meanings."
```

``` ucm
textkit/v2> update
scratch/main> project.create-empty tally
tally/main> builtins.merge lib.builtins
tally/main> branch v1
tally/v1> fork textkit/v1: lib.textkit_1_0_0
```

``` unison
unique type Tally = Tally Label Nat

named : Text -> Tally
named t = Tally (label t) 0

tick : Tally -> Tally
tick = cases Tally l n -> Tally l (n + 1)

total : Tally -> Nat
total = cases Tally _ n -> n

title : Tally -> Label
title = cases Tally l _ -> l

migration : Text
migration = "1.0.0 : Tally, named, tick, total, title. Built on textkit 1.0.0."
```

``` ucm
tally/v1> add
tally/v1> branch /v2
tally/v2> delete.namespace.force lib.textkit_1_0_0
tally/v2> fork textkit/v2: lib.textkit_2_0_0
```

``` unison
unique type Tally = Tally Label Nat

named : Text -> Tally
named t = Tally (label t) 0

tick : Nat -> Tally -> Tally
tick amount = cases Tally l n -> Tally l (n + amount)

total : Tally -> Nat
total = cases Tally _ n -> n

title : Tally -> Label
title = cases Tally l _ -> l

migration : Text
migration = "2.0.0 : tick now takes the amount to add first, so tick t of 1.0.0 is tick 1 t now. Built on textkit 2.0.0: title returns its Label."
```

``` ucm
tally/v2> update
scratch/main> fork textkit/v1: lib.textkit_1_0_0
scratch/main> fork tally/v1: lib.tally_1_0_0
```

The project's own code, written against the 1.0.0 releases.

``` unison
excite : Text -> Text
excite name = shout (label name)

countThree : Nat
countThree = total (tick (tick (tick (named "three"))))

heading : Text -> Text
heading name = shout (title (named name))

report : Text -> Text
report name = excite name ++ " " ++ Nat.toText countThree

excite.tests.hi = test.verify do ensureEqual (excite "hi") "hi!"
countThree.tests.three = test.verify do ensureEqual countThree 3
heading.tests.top = test.verify do ensureEqual (heading "top") "top!"
report.tests.go = test.verify do ensureEqual (report "go") "go! 3"
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> fork textkit/v2: lib.textkit_2_0_0
scratch/main> fork tally/v2: lib.tally_2_0_0
scratch/main> list lib
```
