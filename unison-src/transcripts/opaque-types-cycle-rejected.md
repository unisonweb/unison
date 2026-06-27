# Opaque types cannot be recursive in their RHS

The RHS of an opaque type may not mention the LHS. Recursive types
still belong to ordinary `type` declarations with constructors.

```ucm :hide
> builtins.mergeio
```

A self-referential opaque type is rejected: `T` mentions itself in
the RHS.

```unison :error
opaque type T = T where
  noop : T -> T
  noop x = x
```

A mutual opaque cycle is also rejected: `A`'s RHS mentions `B`,
which would form a cycle if `B`'s RHS mentioned `A`.

```unison :error
opaque type A = B where
  toB : A -> B
  toB a = a

opaque type B = A where
  toA : B -> A
  toA b = b
```
