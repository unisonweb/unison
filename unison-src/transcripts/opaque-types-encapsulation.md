# Opaque types are rigid outside their body

Outside the `where` block of an opaque type, the opaque type does not
unify with its underlying representation. Code that tries to use a
`Float` literal where a `Logarithm` is expected (or vice versa) is
rejected by the typechecker, even though the runtime representation
is identical.

```ucm :hide
> builtins.mergeio
```

```unison
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l
```

```ucm
> add
```

Inside the body, `Float` and `Logarithm` unify (which is why `toFloat l
= l` typechecks). Outside, they don't — this file is rejected.

```unison :error
f : Logarithm
f = 0.5
```

And the reverse direction is also rejected: a `Logarithm` may not be
passed where a `Float` is expected.

```unison :error
g : Float
g = Logarithm.fromFloat 2.0
```
