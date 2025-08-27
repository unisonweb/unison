``` unison

f : (Nat, Nat, Nat) -> Nat
f = cases (x, y, z) -> x + y + z

g : Map Nat ((Nat, Nat, Nat) -> Nat) -> Text
g m = match Map.get 0 m with
  Some f -> Nat.toText (f (1, 2, 3))
  None -> "problem"

m : Map Nat ((Nat, Nat, Nat) -> Nat)
m = Bin 1 0 f Tip Tip

mkTestCase = do
  saveTestCase None "case-05" "v4" g m
  saveTestCase (Some 5) "case-05" "v5" g m
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + f          : (Nat, Nat, Nat) -> Nat
  + g          : Map Nat ((Nat, Nat, Nat) ->{g} Nat) ->{g} Text
  + m          : Map Nat ((Nat, Nat, Nat) ->{g} Nat)
  + mkTestCase : '{IO, Exception} ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> run mkTestCase

  ()
```
