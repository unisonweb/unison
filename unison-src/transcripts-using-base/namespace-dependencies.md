# namespace.dependencies command

```ucm:hide
.external> builtins.merge
```

```unison:hide
external.mynat = 1
mynamespace.dependsOnText = external.mynat Nat.+ 10
```

```ucm
.> add
.mynamespace> namespace.dependencies
```
