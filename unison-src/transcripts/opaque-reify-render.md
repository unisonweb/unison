# Opaque types render watch results via their `reify` body fn

When an opaque type declares a body fn named `reify` of type
`T α* ->{} '(T α*)`, watch results of opaque type are routed through it
so the rendered Unison source reflects the user's smart constructors
rather than the underlying representation.

```ucm :hide
> builtins.mergeio
```

## Phase 9b — outermost opaque rendering

A monomorphic opaque type with a `reify` body fn. The watch displays
`fromFloat 0.0`, the body of the thunk produced by `Logarithm.reify`,
instead of the raw `Float`.

```unison
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

## An opaque type without `reify` falls back to the underlying value

The opaque-ness is still hidden — outside the body, `Token` ≠ `Text` —
but at the display layer, without a registered `reify` we render the
raw representation.

```unison
opaque type Token = Text where
  fromText : Text -> Token
  fromText t = t

> Token.fromText "abc"
```
