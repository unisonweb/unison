# Opaque types are rigid outside their body

Outside the `where` block of an opaque type, the opaque type does not
unify with its underlying representation. Code that tries to use a
`Float` literal where a `Logarithm` is expected (or vice versa) is
rejected by the typechecker, even though the runtime representation
is identical.

``` ucm :hide
> builtins.mergeio
```

``` unison
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Logarithm.fromFloat : Float -> #jqtsjdc9s6
  + Logarithm.toFloat   : #jqtsjdc9s6 -> Float

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Done.
```

Inside the body, `Float` and `Logarithm` unify (which is why `toFloat l = l` typechecks). Outside, they don't — this file is rejected.

``` unison :error
f : Logarithm
f = 0.5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Float
  where I expected to find:  Logarithm

      1 | f : Logarithm
      2 | f = 0.5

    from right here:

      2 | f = 0.5
```

And the reverse direction is also rejected: a `Logarithm` may not be
passed where a `Float` is expected.

``` unison :error
g : Float
g = Logarithm.fromFloat 2.0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Logarithm
  where I expected to find:  Float

      1 | g : Float
      2 | g = Logarithm.fromFloat 2.0
```
