# TypeTag

Tests for the `TypeTag` built-in type which carries a structured
representation of a monomorphic type at runtime, synthesized
automatically via the implicit (`=>`) system.

``` ucm :hide
> builtins.mergeio
```

## Basic synthesis for ground types

A function that demands a `TypeTag Nat` implicit can be called
at a site where the type is known.

``` unison
useTag : TypeTag Nat => Text
useTag = TypeTag.toText summon
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + useTag : TypeTag Nat => Text

  Run `update` to apply these changes to your codebase.
```

## Polymorphic threading

A function with `TypeTag a =>` can pass its tag to callees.

``` unison
inner : TypeTag a => Text
inner = TypeTag.toText summon

outer : TypeTag a => Text
outer = inner
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + inner : TypeTag a => Text
  + outer : TypeTag a => Text

  Run `update` to apply these changes to your codebase.
```

## toText rendering

``` unison
> TypeTag.toText (summon : TypeTag Nat)
> TypeTag.toText (summon : TypeTag [Nat])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > TypeTag.toText (summon : TypeTag Nat)
          ⧩
          "Nat"

    2 | > TypeTag.toText (summon : TypeTag [Nat])
          ⧩
          "Sequence Nat"
```

## Debug.toText rendering

``` unison
debugTag : TypeTag a => a -> Text
debugTag _ =
  match Debug.toText (summon : TypeTag a) with
    None -> bug "no debug text"
    Some (Left t) -> t
    Some (Right t) -> t

> debugTag 42
> debugTag [1, 2, 3]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + debugTag : TypeTag a => a -> Text

  Run `update` to apply these changes to your codebase.

    8 | > debugTag 42
          ⧩
          "TypeTag Nat"

    9 | > debugTag [1, 2, 3]
          ⧩
          "TypeTag (List Nat)"
```

## Value round-trip

``` unison
loadTag : TypeTag Nat
loadTag =
  v = Value.value (summon : TypeTag Nat)
  match unsafe.coerceAbilities Value.load v with
    Right t -> t
    Left _ -> bug "Value.load failed"

> TypeTag.toText loadTag
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + loadTag : TypeTag Nat

  Run `update` to apply these changes to your codebase.

    8 | > TypeTag.toText loadTag
          ⧩
          "Nat"
```
