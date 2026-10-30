<!-- expected: outcome FAIL -->
<!-- The agent never runs upgrade: it edits the code by hand against 2.0.0 (names have to be qualified, since both versions are in lib) and leaves 1.0.0 in lib. The behavior is right, the old copy is still there. -->

``` unison
describe : mylib_2_0_0.Opt -> Text
describe = cases mylib_2_0_0.Opt.Opt n _ -> "repeat " Text.++ Nat.toText n

shout : Nat -> Text -> Text
shout n name = mylib_2_0_0.render (mylib_2_0_0.Opt.Opt n " ") name

twice : mylib_2_0_0.Opt
twice = mylib_2_0_0.Opt.Opt 2 " "

welcome : Text -> Text
welcome name = mylib_2_0_0.greet "Hello" name
```

``` ucm
scratch/main> update
scratch/main> test
scratch/main> list lib
```
