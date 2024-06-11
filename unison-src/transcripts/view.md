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
scratch/main a> view thing
-- view.global should search globally and be absolutely qualified
scratch/main a> view.global thing
-- Should support absolute paths outside of current namespace
scratch/main a> view .b.thing
```
