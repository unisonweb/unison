# Opaque types render watch results via their `reify` body fn

When an opaque type declares a body fn named `reify` of type
`T α* ->{} '(T α*)`, watch results of opaque type are routed through it
so the rendered Unison source reflects the user's smart constructors
rather than the underlying representation.

``` ucm :hide
> builtins.mergeio
```

## Phase 9b — outermost opaque rendering

A monomorphic opaque type with a `reify` body fn. The watch displays
`fromFloat 0.0`, the body of the thunk produced by `Logarithm.reify`,
instead of the raw `Float`.

``` unison
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l

  reify : Logarithm ->{} '(Logarithm)
  reify l =
    f = toFloat l
    do Logarithm.fromFloat f

> Logarithm.fromFloat 1.0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Logarithm.fromFloat : Float -> #a2ldi1bckv
  + Logarithm.reify     : #a2ldi1bckv -> '#a2ldi1bckv
  + Logarithm.toFloat   : #a2ldi1bckv -> Float

  Run `update` to apply these changes to your codebase.

    13 | > Logarithm.fromFloat 1.0
           ⧩
           fromFloat 0.0
```

## An opaque type without `reify` falls back to the underlying value

The opaque-ness is still hidden — outside the body, `Token` ≠ `Text` —
but at the display layer, without a registered `reify` we render the
raw representation.

``` unison
opaque type Token = Text where
  fromText : Text -> Token
  fromText t = t

> Token.fromText "abc"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Token.fromText : Text -> #qs8dhndfis

  Run `update` to apply these changes to your codebase.

    5 | > Token.fromText "abc"
          ⧩
          "abc"
```
