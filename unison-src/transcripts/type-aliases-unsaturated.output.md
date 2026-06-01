# Unsaturated alias use is rejected

``` ucm :hide
> builtins.mergeio
```

`Endo` has arity 1. Using `Endo` without an argument inside a type
position is rejected by the kindchecker — `Endo` has kind `* -> *` but
the context requires `*`.

``` unison :error
type alias Endo a = a -> a
type Box = Box Endo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        2 | type Box = Box Endo
    
    The arrow type (->) expects arguments of kind Type; however,
    it is applied to #degd6fsii4 which has kind: Type -> Type.
```
