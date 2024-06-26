# namespace.dependencies command

```unison
const a b = a
external.mynat = 1
mynamespace.dependsOnText = const external.mynat 10
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    const                     : a -> b -> a
    external.mynat            : ##Nat
    mynamespace.dependsOnText : ##Nat

.mynamespace> namespace.dependencies

  External dependency   Dependents in .mynamespace
  ##Nat                 1. dependsOnText
                        
  .const                1. dependsOnText
                        
  .external.mynat       1. dependsOnText

```
