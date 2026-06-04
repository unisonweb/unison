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

## Serialization round-trip

``` unison
roundTrip : TypeTag a => Optional (TypeTag a)
roundTrip = TypeTag.deserialize (TypeTag.serialize summon)

> roundTrip : Optional (TypeTag (List Nat))
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
I got confused here:

    5 | 

I was surprised to find an end of section here.
I was expecting one of these instead:

* ->
* newline or semicolon
```
