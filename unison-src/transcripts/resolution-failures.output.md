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
unique type one.AmbiguousType = one.AmbiguousConstructor
unique type two.AmbiguousType = two.AmbiguousConstructor
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type one.AmbiguousType
      unique type two.AmbiguousType

```
```ucm
  ☝️  The namespace .example.resolution_failures is empty.

.example.resolution_failures> add

  ⍟ I've added these definitions:
  
    unique type one.AmbiguousType
    unique type two.AmbiguousType

```
## Tests

Now we introduce code which isn't sufficiently qualified. 
It is ambiguous which type from which namespace we mean.

```unison
useAmbiguousType : AmbiguousType -> ()
useAmbiguousType _ = ()

useUnknownType : UnknownType -> ()
useUnknownType _ = ()
```

```ucm

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        1 | useAmbiguousType : AmbiguousType -> ()
        2 | useAmbiguousType _ = ()
        3 | 
        4 | useUnknownType : UnknownType -> ()
    
    
    Symbol          Suggestions
                    
    AmbiguousType   one.AmbiguousType
                    two.AmbiguousType
                    
    UnknownType     ?
  

```
