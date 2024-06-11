# namespace.dependencies command

```unison
external.mynat = 1
mynamespace.dependsOnText = external.mynat Nat.+ 10
```

```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    external.mynat            : Nat
    mynamespace.dependsOnText : Nat

  ☝️  The namespace .mynamespace is empty.

.mynamespace> namespace.dependencies

  ⚠️
  
  .mynamespace is an empty namespace.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  .mynamespace is an empty namespace.

