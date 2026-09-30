# Unsaturated alias use is rejected

```ucm :hide
> builtins.mergeio
```

`Endo` has arity 1. Using `Endo` without an argument inside a type
position is rejected by the kindchecker — `Endo` has kind `* -> *` but
the context requires `*`.

```unison :error
type alias Endo a = a -> a
type Box = Box Endo
```
