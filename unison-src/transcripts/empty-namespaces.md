# Empty namespace behaviours

## Operations on empty namespaces

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
