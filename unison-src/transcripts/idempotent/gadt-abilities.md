``` ucm :hide
> builtins.merge lib.builtin
```

# GADT-indexed abilities

An `ability` is a GADT: each operation writes out its own type, so an operation
can pin the ability's type index, just like a data constructor pins a data index.
Here `Eff` is indexed by the type its operations carry — `emitNat` fixes the
index to `Nat`, `emitBool` to `Boolean`:

``` unison
unique ability Eff a where
  emitNat : Nat ->{Eff Nat} ()
  emitBool : Boolean ->{Eff Boolean} ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Eff a

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

## Handling refines the index per branch

Matching an operation in a handler refines the scrutinee's ability index inside
that branch, exactly as matching a constructor refines a data index. In the
`emitNat` branch the index is known to be `Nat`, so `Some n : Optional a`
type-checks; in the `emitBool` branch it is `Boolean`:

``` unison
collect : Request {Eff a} r -> Optional a
collect = cases
  { _ } -> None
  { emitNat n -> _ } -> Some n
  { emitBool b -> _ } -> Some b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + collect : Request {Eff a} r -> Optional a

  Run `update` to apply these changes to your codebase.
```

## A concrete index rules operations out

When the handled ability's index is known concretely, operations that can't occur
at that index are impossible and need not be handled (coverage is index-aware).
The scrutinee here is `Eff Nat`, so `emitBool` (which performs `Eff Boolean`) is
excluded, and the handler is exhaustive without it:

``` unison
onlyNat : Request {Eff Nat} r -> Nat
onlyNat = cases
  { _ } -> 0
  { emitNat n -> _ } -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + onlyNat : Request {Eff Nat} r -> Nat

  Run `update` to apply these changes to your codebase.
```

## Several abilities at once

A handler may handle a whole row of abilities. Matching an `Eff` operation
refines only `Eff`'s index, leaving the other abilities in the row alone:

``` unison
unique ability Log where
  log : Text ->{Log} ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Log

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` unison
collectL : Request {Eff a, Log} r -> Optional a
collectL = cases
  { _ } -> None
  { log _ -> _ } -> None
  { emitNat n -> _ } -> Some n
  { emitBool b -> _ } -> Some b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + collectL : Request {Log, Eff a} r -> Optional a

  Run `update` to apply these changes to your codebase.
```
