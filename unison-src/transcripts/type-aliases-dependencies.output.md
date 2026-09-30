# Aliases and declarations share dependency ordering

Aliases can form chains through local data declarations, and another data
constructor can use the resulting alias. Source order does not matter.

``` ucm :hide
> builtins.mergeio
```

``` unison
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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias AlphabeticalFirst = Zebra
  + type alias Twice = Wrapped
  + type alias Wrapped = Box
  + type alias Zebra = Nat
  + structural type Box
  + structural type Outer

  + outer  : Outer
  + unwrap : Twice -> Nat
  + value  : Twice

  Run `update` to apply these changes to your codebase.

    18 | > unwrap value
           ⧩
           42
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view Twice

  type alias Twice = Wrapped

> view Outer

  structural type Outer = Outer Twice
```

Stored alias chains must also kindcheck and expand in a later file.

``` unison
> unwrap (Box.Box 43)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > unwrap (Box.Box 43)
          ⧩
          43
```

A cycle containing an alias cannot be assigned independent content hashes.

``` unison :error
structural type Recursive = Recursive Back
type alias Back = Recursive
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a cycle among these `type alias` declarations: Back ,
  Recursive

      2 | type alias Back = Recursive


  Type aliases cannot be recursive — use a `type` declaration
  instead.
```
