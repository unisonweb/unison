# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

First we define differing types with the same name in different namespaces:

```ucm
.> cd example.resolve

  ☝️  The namespace .example.resolve is empty.

```
Now let's add a term named `a.foo`:

```unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

unique type T = A | B
one.ambiguousTerm : T
one.ambiguousTerm = A
two.ambiguousTerm : T
two.ambiguousTerm = B
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type T
      unique type one.AmbiguousType
      unique type two.AmbiguousType
      one.ambiguousTerm : T
      two.ambiguousTerm : T

```
```ucm
  ☝️  The namespace .example.resolution_failures is empty.

.example.resolution_failures> add

  ⍟ I've added these definitions:
  
    unique type T
    unique type one.AmbiguousType
    unique type two.AmbiguousType
    one.ambiguousTerm : T
    two.ambiguousTerm : T

```
## Tests

Now we introduce code which isn't sufficiently qualified. 
It is ambiguous which type from which namespace we mean.

```unison
useAmbiguousType : Ambiguous -> ()
useAmbiguousType _ = ()
```

```ucm

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        1 | useAmbiguousType : Ambiguous -> ()
    
    🔍 The following name could not be found: Ambiguous
  

```
```unison
useAmbiguousTerm : T
useAmbiguousTerm = ambiguousTerm
```

```ucm

  I'm not sure what ambiguousTerm means at line 2, columns 20-33
  
      2 | useAmbiguousTerm = ambiguousTerm
  
  Whatever it is, it has a type that conforms to T.
  I found some terms in scope that have matching names and types. Maybe you meant one of these:
  
    - one.ambiguousTerm : T
    - two.ambiguousTerm : T

```
