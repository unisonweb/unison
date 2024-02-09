# namespace.dependencies command

```unison
external.mynat = 1
mynamespace.dependsOnText = external.mynat Nat.+ 10
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    external.mynat            : Nat
    mynamespace.dependsOnText : Nat

.mynamespace> namespace.dependencies

  External dependency   Dependents in .mynamespace
  .builtin.Nat          1. dependsOnText
                        
  .builtin.Nat.+        1. dependsOnText
                        
  .external.mynat       1. dependsOnText

```
