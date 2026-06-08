# A reify body fn with the wrong signature is rejected

A body fn named `reify` in an opaque type declaration must have the
canonical signature `T α* ->{} '(T α*)`. Any other signature is
rejected by the typechecker (Phase 9a).

``` ucm :hide
> builtins.mergeio
```

The body fn is named `reify` but its type is `T -> T` rather than
`T ->{} '(T)`, so this file is rejected.

``` unison :error
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l

  reify : Logarithm -> Logarithm
  reify l = l
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  The `reify` body fn of an opaque type must have a specific
  signature, but ` Logarithm.reify ` did not match.

      8 |   reify : Logarithm -> Logarithm


  Expected:
    #5fpj4irsse -> Unit -> #5fpj4irsse

  Got:
    #5fpj4irsse -> #5fpj4irsse

  The expected shape is `forall a*. T a* ->{} '(T a*)` — a pure
  function from the opaque type to a thunk producing it.
```

A reify that returns the wrong type (returns the underlying repr
instead of a thunk of the opaque type) is similarly rejected.

``` unison :error
opaque type Celsius = Float where
  fromFloat : Float -> Celsius
  fromFloat x = x

  reify : Celsius ->{} Float
  reify c = c
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  The `reify` body fn of an opaque type must have a specific
  signature, but ` Celsius.reify ` did not match.

      5 |   reify : Celsius ->{} Float


  Expected:
    #1u63grnua0 -> Unit -> #1u63grnua0

  Got:
    #1u63grnua0 -> Float

  The expected shape is `forall a*. T a* ->{} '(T a*)` — a pure
  function from the opaque type to a thunk producing it.
```
