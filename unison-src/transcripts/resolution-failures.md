# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

```ucm
scratch/main> builtins.merge lib.builtins
```

First we define differing types with the same name in different namespaces:

```unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

one.ambiguousTerm = "term one"
two.ambiguousTerm = "term two"
```

```ucm
scratch/main> add
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
useAmbiguousTerm = ambiguousTerm
```
