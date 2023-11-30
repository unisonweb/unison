# namespace.dependencies command

```unison
myMetadata = "just some text"
```

```unison
dependsOnNat = 1
dependsOnInt = -1
dependsOnIntAndNat = Nat.drop 1 10
hasMetadata = 3
```

```ucm
  ☝️  The namespace .dependencies is empty.

.dependencies> add

  ⍟ I've added these definitions:
  
    dependsOnInt       : Int
    dependsOnIntAndNat : Nat
    dependsOnNat       : Nat
    hasMetadata        : Nat

.dependencies> link .metadata.myMetadata hasMetadata

```

```ucm
.dependencies> add.dependencies> link .metadata.myMetadata hasMetadata.dependencies> namespace.dependencies
```


🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to link. Type `help` or `?` to get help.
