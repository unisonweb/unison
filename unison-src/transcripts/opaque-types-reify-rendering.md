# Reify body fns control how opaque values render

When a monomorphic opaque type declares a body fn

    reify : T ->{} '(T)

the display layer routes any watch result of type `T` through that body
fn, then decompiles the resulting thunk to recover a Unison expression
that reconstructs the value. The rendered output uses the user's smart
constructor names rather than the underlying representation.

```ucm :hide
> builtins.mergeio
```

```unison
opaque type Celsius = Float where
  fromFloat : Float -> Celsius
  fromFloat f = f

  toFloat : Celsius -> Float
  toFloat c = c

  reify : Celsius ->{} '(Celsius)
  reify c =
    raw = toFloat c
    do Celsius.fromFloat raw

> Celsius.fromFloat 21.0
```

The watch result reads as `fromFloat 21.0`, the reconstructed form, not
the raw `Float`.

Compare with an opaque type that does *not* declare a `reify`. Without
a registered reify, the display layer falls back to rendering the
underlying representation.

```unison
opaque type Token = Text where
  fromText : Text -> Token
  fromText t = t

> Token.fromText "abc"
```

The watch shows `"abc"`, the raw `Text`, because there is no reify to
route through.
