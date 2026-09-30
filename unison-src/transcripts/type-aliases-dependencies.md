# Aliases and declarations share dependency ordering

Aliases can form chains through local data declarations, and another data
constructor can use the resulting alias. Source order does not matter.

```ucm:hide
> builtins.mergeio
```

```unison
structural type Outer = Outer Twice
type alias Twice = Wrapped
type alias Wrapped = Box

-- Alphabetical order differs from dependency order.
type alias Zebra = Nat
type alias AlphabeticalFirst = Zebra
structural type Box = Box Nat

value : Twice
value = Box.Box 42
outer = Outer.Outer value

unwrap : Twice -> Nat
unwrap b = match b with
  Box.Box n -> n

> unwrap value
```

```ucm
> add
> view Twice
> view Outer
```

Stored alias chains must also kindcheck and expand in a later file.

```unison
> unwrap (Box.Box 43)
```

A cycle containing an alias cannot be assigned independent content hashes.

```unison:error
structural type Recursive = Recursive Back
type alias Back = Recursive
```
