# A reify body fn with the wrong signature is rejected

A body fn named `reify` in an opaque type declaration must have the
canonical signature `T α* ->{} '(T α*)`. Any other signature is
rejected by the typechecker (Phase 9a).

```ucm :hide
> builtins.mergeio
```

The body fn is named `reify` but its type is `T -> T` rather than
`T ->{} '(T)`, so this file is rejected.

```unison :error
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l

  reify : Logarithm -> Logarithm
  reify l = l
```

A reify that returns the wrong type (returns the underlying repr
instead of a thunk of the opaque type) is similarly rejected.

```unison :error
opaque type Celsius = Float where
  fromFloat : Float -> Celsius
  fromFloat x = x

  reify : Celsius ->{} Float
  reify c = c
```
