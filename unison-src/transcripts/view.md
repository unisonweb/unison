# View commands

```ucm:hide
scratch/main> builtins.merge
```

```unison:hide
a.thing = "a"
b.thing = "b"
```

```ucm:hide
scratch/main> add
```

```ucm
-- Should suffix-search and find values in sub-namespaces
scratch/main> view thing
-- Should be local to namespace
.a> view thing
-- view.global should search globally and be absolutely qualified
.a> view.global thing
-- Should support absolute paths outside of current namespace
.a> view .b.thing
```
