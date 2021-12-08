# Empty namespace behaviours

```unison:hide
mynamespace.x = 1
```

```ucm:hide
.> add
.> delete.namespace mynamespace
```

The deleted namespace shouldn't appear in `ls` output.
```ucm:error
.> ls
```
```ucm:error
.> ls.verbose
```
```ucm:error
.> find mynamespace
```


The history of the namespace should still exist if requested explicitly.

```ucm
.> history mynamespace
```

Merging an empty namespace should still copy its history if it has some.

```ucm
.empty> history
.empty> merge .mynamespace
.empty> history
```
