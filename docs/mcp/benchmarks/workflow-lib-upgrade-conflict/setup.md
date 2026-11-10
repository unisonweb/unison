The library `mylib` stands in for a Share library: a local project forked into `lib`, so this setup needs no network.

``` ucm
scratch/main> project.create-empty mylib
mylib/main> builtins.merge lib.builtins
mylib/main> branch v1
```

``` unison
unique type Opt = Opt Nat

greet : Text -> Text
greet name = "Hello, " ++ name

render : Opt -> Text -> Text
render opt name =
  go n = if n == 0 then "" else if n == 1 then greet name else greet name ++ " " ++ go (drop n 1)
  match opt with Opt n -> go n

migration : Text
migration = "1.0.0 : Opt n, greet name. render repeats the greeting n times with one space between the copies."
```

``` ucm
mylib/v1> add
mylib/v1> branch /v2
```

``` unison
unique type Opt = Opt Nat Text

greet : Text -> Text -> Text
greet salutation name = salutation ++ ", " ++ name

render : Opt -> Text -> Text
render opt name =
  match opt with Opt n sep ->
    go k = if k == 0 then "" else if k == 1 then greet "Hello" name else greet "Hello" name ++ sep ++ go (drop k 1)
    go n

migration : Text
migration = "2.0.0 : Opt n sep, greet salutation name. Opt n of 1.0.0 is Opt n \" \" now, and greet name of 1.0.0 is greet \"Hello\" name now."
```

``` ucm
mylib/v2> update
scratch/main> fork mylib/v1: lib.mylib_1_0_0
```

The project's own code, written against 1.0.0.

``` unison
welcome : Text -> Text
welcome name = greet name

twice : Opt
twice = Opt 2

shout : Nat -> Text -> Text
shout n name = render (Opt n) name

describe : Opt -> Text
describe opt = match opt with Opt n -> "repeat " ++ Nat.toText n

welcome.tests.bob = test.verify do ensureEqual (welcome "bob") "Hello, bob"
shout.tests.two = test.verify do ensureEqual (shout 2 "bob") "Hello, bob Hello, bob"
describe.tests.twice = test.verify do ensureEqual (describe twice) "repeat 2"
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> fork mylib/v2: lib.mylib_2_0_0
scratch/main> list lib
```
