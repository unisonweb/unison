# namespace.dependencies command

```ucm
scratch/main> builtins.merge lib.builtins
```

```unison:hide
const a b = a
external.mynat = 1
mynamespace.dependsOnText = const external.mynat 10
```

```ucm
scratch/main> add
scratch/main> namespace.dependencies mynamespace
```
