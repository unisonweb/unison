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
-- view.global should search globally and be absolutely qualified
scratch/other> view.global thing
-- Should support absolute paths
scratch/main> view .b.thing
-- Should support branch relative paths
scratch/other> view /main:.a.thing
```
