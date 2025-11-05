``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

``` unison :error
missingDo : 'Nat
missingDo = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Nat
  where I expected to find:  Unit -> Nat

      1 | missingDo : 'Nat
      2 | missingDo = 2

    from right here:

      2 | missingDo = 2
  I expected the expression to be delayed, but it was not.
  Are you missing a `do`?
```

``` unison :error
superfluousDo : Nat
superfluousDo = do
  2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Unit ->{𝕖} Nat
  where I expected to find:  Nat

      1 | superfluousDo : Nat
      2 | superfluousDo = do
      3 |   2

    from right here:

      3 |   2

  I didn't expect this expression to be delayed, but it was.
  Are you using a `do` where you don't need one,
  or are you missing a `()` to force an expression?
```

``` unison :error
varPlusOne var = do
  Ref.read var Nat.+ 1

aBlock : '{IO} Nat
aBlock = do
  var = IO.ref 10
  -- Missing delay
  varPlusOne var
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Unit ->{IO} Nat
  where I expected to find:  Nat

      4 | aBlock : '{IO} Nat
      5 | aBlock = do
      6 |   var = IO.ref 10
      7 |   -- Missing delay

    from right here:

      2 |   Ref.read var Nat.+ 1

  I didn't expect this expression to be delayed, but it was.
  Are you using a `do` where you don't need one,
  or are you missing a `()` to force an expression?
```
