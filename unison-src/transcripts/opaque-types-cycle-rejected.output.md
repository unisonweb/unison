# Opaque types cannot be recursive in their RHS

The RHS of an opaque type may not mention the LHS. Recursive types
still belong to ordinary `type` declarations with constructors.

``` ucm :hide
> builtins.mergeio
```

A self-referential opaque type is rejected: `T` mentions itself in
the RHS.

``` unison :error
opaque type T = T where
  noop : T -> T
  noop x = x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a cycle among these `opaque type` declarations: T

      1 | opaque type T = T where
      2 |   noop : T -> T
      3 |   noop x = x


  An opaque type's RHS cannot mention itself or form a cycle
  with another opaque type.
```

A mutual opaque cycle is also rejected: `A`'s RHS mentions `B`,
which would form a cycle if `B`'s RHS mentioned `A`.

``` unison :error
opaque type A = B where
  toB : A -> B
  toB a = a

opaque type B = A where
  toA : B -> A
  toA b = b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a cycle among these `opaque type` declarations: A , B

      1 | opaque type A = B where
      2 |   toB : A -> B
      3 |   toB a = a


  An opaque type's RHS cannot mention itself or form a cycle
  with another opaque type.
```
