# TypeTag

Tests for the `TypeTag` built-in type which carries a structured
representation of a monomorphic type at runtime, synthesized
automatically via the implicit (`=>`) system.

``` ucm :hide
> builtins.mergeio
```

## Basic synthesis for ground types

``` unison
useTag : TypeTag Nat -> Text
useTag tag = TypeTag.toText tag

getNatTag : Text
getNatTag = useTag (summon : TypeTag Nat)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getNatTag : Text
  + useTag    : TypeTag Nat -> Text

  Run `update` to apply these changes to your codebase.
```

## Applied types

``` unison
listTag : TypeTag (List Nat) -> Text
listTag tag = TypeTag.toText tag

getListNatTag : Text
getListNatTag = listTag (summon : TypeTag (List Nat))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getListNatTag : Text
  + listTag       : TypeTag [Nat] -> Text

  Run `update` to apply these changes to your codebase.
```

## Polymorphic threading

A function with `TypeTag a =>` can pass its tag through to callees.

``` unison
inner : TypeTag a => Text
inner = TypeTag.toText (summon : TypeTag a)

outer : TypeTag a => Text
outer = inner
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + inner : TypeTag a => Text
  + outer : TypeTag a => Text

  Run `update` to apply these changes to your codebase.
```

## Extracting references

``` unison
getRefs : List Link.Type
getRefs = TypeTag.references (summon : TypeTag (List Nat))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getRefs : [Link.Type]

  Run `update` to apply these changes to your codebase.
```

## Equality via Universal.==

``` unison
tagsEqual : Boolean
tagsEqual =
  t1 = summon : TypeTag Nat
  t2 = summon : TypeTag Nat
  t1 == t2

tagsDifferent : Boolean
tagsDifferent =
  t1 = summon : TypeTag Nat
  t2 = summon : TypeTag Int
  t1 == t2
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
I got confused here:

    4 |   t2 = summon : TypeTag Nat


I was surprised to find an end of section here.
I was expecting one of these instead:

* ->
* newline or semicolon
```
