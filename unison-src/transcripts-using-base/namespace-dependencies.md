# namespace.dependencies command

```ucm:hide
.> builtins.merge
```

```unison:hide
myMetadata = "just some text"
```

```ucm:hide
.metadata> add
```

```unison:hide
dependsOnNat = 1
dependsOnInt = -1
dependsOnIntAndNat = Nat.drop 1 10
hasMetadata = 3
```

```ucm
.dependencies> add
.dependencies> link .metadata.myMetadata hasMetadata 
.dependencies> namespace.dependencies
```
