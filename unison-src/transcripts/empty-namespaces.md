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

## history

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

Add and then delete a term to add some history to a deleted namespace.

```unison:hide
deleted.x = 1
stuff.thing = 2
```

```ucm:hide
.> add
.> delete.namespace .deleted
```

I should be allowed to fork over a deleted namespace

```ucm
.> fork stuff deleted
```

The history from the `deleted` namespace should have been overwritten by the history from `stuff`.

```ucm
.> history stuff
.> history deleted
```
