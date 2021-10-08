# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

First we define differing types with the same name in different namespaces:

```ucm
.> cd example.resolution_failures
```

Now let's add a term named `a.foo`:

```unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

one.ambiguousTextTerm = "term one"
two.ambiguousTextTerm = "term two"

unique type A = One | Two
unique type B = B

one.ambiguousTermWithDifferingTypes = One
two.ambiguousTermWithDifferingTypes = Two
three.ambiguousTermWithDifferingTypes = B
```

```ucm
.example.resolution_failures> add
```

## Tests

Now we introduce code which isn't sufficiently qualified. 
It is ambiguous which type from which namespace we mean.

We expect the output to:

1. Print all ambiguous usage sites separately
2. Print possible disambiguation suggestions for each unique ambiguity

```unison:error
-- We intentionally avoid using a constructor to ensure the constructor doesn't
-- affect type resolution.
useAmbiguousType : AmbiguousType -> ()
useAmbiguousType _ = ()

useUnknownType : UnknownType -> ()
useUnknownType _ = ()

-- Despite being a duplicate disambiguation, this should still be included in the annotations printout
separateAmbiguousTypeUsage : AmbiguousType -> ()
separateAmbiguousTypeUsage _ = ()
```

Currently, ambiguous terms are caught and handled by type directed name resolution,
but expect it to eventually be handled by the above machinery.

```unison:error
useAmbiguousTextTermNoTypeHint = ambiguousTextTerm
```

```unison:error
useAmbiguousTermWithDifferingTypes = ambiguousTermWithDifferingTypes
```

```unison:error
useAmbiguousTermWithDifferingTypesWithTypeHint : A
useAmbiguousTermWithDifferingTypesWithTypeHint 
  = ambiguousTermWithDifferingTypes
```
