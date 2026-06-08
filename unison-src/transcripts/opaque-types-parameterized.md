# Opaque types with type parameters

Opaque types may take type parameters; both the RHS and the body fns
can mention them. Outside the body, `Box a` is rigid against `a`; inside,
they unify.

```ucm :hide
> builtins.mergeio
```

```unison
opaque type Box a = a where
  wrap : a -> Box a
  wrap x = x

  unwrap : Box a -> a
  unwrap b = b

> Box.wrap 42
> Box.unwrap (Box.wrap "hi")
```

`Box.wrap 42` produces a value of type `Box Nat`; `Box.unwrap` returns
the underlying value. The watch lines confirm both directions evaluate
correctly at runtime.

```ucm
> add
```

The body fns are stored in the codebase and can be used from a fresh
unison block.

```unison
example : Box Text
example = Box.wrap "stored"

extracted : Text
extracted = Box.unwrap example
```

Outside the body, the parameterized opaque type is rigid: this fails
because `Box Nat` is not the same as `Nat`.

```unison :error
bad : Box Nat
bad = 7
```
