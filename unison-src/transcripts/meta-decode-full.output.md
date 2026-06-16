# Meta.compile round-trips If/Match/LetRec/Bytes

Earlier versions of `MetaCompile` errored on `If`, `LetRec`, `Match`, and
`Bytes` literal nodes. This transcript exercises each by decompiling a
term that uses one of these forms and handing it to `Meta.typecheck`.

``` ucm :hide
scratch/main> builtins.mergeio
```

A `match` expression — exercises Match + Pattern decoders.

``` unison
classify : Nat -> Text
classify n = match n with
  0 -> "zero"
  1 -> "one"
  _ -> "many"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + classify : Nat -> Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

An `if` expression — exercises If decoder.

``` unison
condFlip : Boolean -> Nat
condFlip b = if b then 1 else 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + condFlip : Boolean -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Mutually recursive `let rec` bindings — exercises LetRec decoder.

``` unison
letRecPair : Nat -> Nat
letRecPair n =
  step k = succ k
  succ k = step k
  n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + letRecPair : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

A bytes literal — exercises the Bytes literal decoder (lowered to
`Bytes.fromList`).

``` unison
someBytes : Bytes
someBytes = 0xs0123abcd
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + someBytes : Bytes

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Decompile each term and typecheck the result.

``` unison
roundTrip : '{IO} Either Text Text
roundTrip _ =
  cTm = Meta.decompile classify
  fTm = Meta.decompile condFlip
  lTm = Meta.decompile letRecPair
  bTm = Meta.decompile someBytes
  match Meta.typecheck cTm with
    Left e -> Left ("classify: " ++ e)
    Right _ -> match Meta.typecheck fTm with
      Left e -> Left ("condFlip: " ++ e)
      Right _ -> match Meta.typecheck lTm with
        Left e -> Left ("letRecPair: " ++ e)
        Right _ -> match Meta.typecheck bTm with
          Left e -> Left ("someBytes: " ++ e)
          Right _ -> Right "all four decoded and typechecked"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + roundTrip : '{IO} Either Text Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run roundTrip

  Right "all four decoded and typechecked"
```
