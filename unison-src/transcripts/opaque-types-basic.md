# Opaque types: basic declaration and runtime

A monomorphic opaque type declares a new nominal type whose underlying
representation is a base type, plus a designated set of body functions
that may freely cross between the opaque type and its representation.
At runtime the value *is* the representation; the opaque identity is
only visible at the typecheck / codebase level.

The body block may include a `reify : T ->{} '(T)` body fn that the
display layer uses to render values; when present, watch results are
routed through it (see Phase 9b).

```ucm :hide
> builtins.mergeio
```

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

The watch evaluates at runtime; the rendered output reflects the
`reify`-produced form rather than the underlying `Float`.

```ucm
> add
```

After `add`, both the opaque type itself and its body fns appear in the
namespace. The body fns live under `Logarithm.` and are reachable as
ordinary names; the opaque type's identity is enforced at typecheck time.

```ucm
> ls Logarithm
```

The same body fns can be called from a fresh unison block, with the
opaque type loaded from the codebase rather than declared inline.

```unison
example : Logarithm
example = Logarithm.fromFloat 2.0

exampleFloat : Float
exampleFloat = Logarithm.toFloat example
```
