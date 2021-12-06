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
