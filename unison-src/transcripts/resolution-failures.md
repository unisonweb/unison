# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

First we define differing types with the same name in different namespaces:

```ucm
.> cd example.resolve
```

Now let's add a term named `a.foo`:

```unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

one.ambiguousTerm : .base.Nat
one.ambiguousTerm = 1
two.ambiguousTerm : .base.Nat
two.ambiguousTerm = 2
```

```ucm
.example.resolution_failures> add
```

## Tests

Now we introduce code which isn't sufficiently qualified. 
It is ambiguous which type from which namespace we mean.

```unison
useAmbiguousType : Ambiguous -> ()
useAmbiguousType _ = ()

useAmbiguousTerm : Nat
useAmbiguousTerm = ambiguousTerm
```
