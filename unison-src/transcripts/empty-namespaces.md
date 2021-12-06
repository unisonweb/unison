# Empty namespace behaviours

```ucm:hide
.> builtins.merge
```

```unison
mynamespace.x = 1
```

```ucm:hide
.> add
.> delete.namespace mynamespace
```

The deleted namespace shouldn't appear in `ls` output.
```ucm
.> ls
```
