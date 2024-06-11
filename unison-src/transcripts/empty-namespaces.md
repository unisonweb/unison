# Empty namespace behaviours

```unison:hide
mynamespace.x = 1
```

```ucm:hide
scratch/main> add
scratch/main> delete.namespace mynamespace
```

The deleted namespace shouldn't appear in `ls` output.
```ucm:error
scratch/main> ls
```
```ucm:error
scratch/main> find.verbose
```
```ucm:error
scratch/main> find mynamespace
```

## history

The history of the namespace should be empty.

```ucm
scratch/main> history mynamespace
```

Merging an empty namespace should be a no-op

```ucm:error
.empty> history
.empty> merge.old .mynamespace
.empty> history
```

Add and then delete a term to add some history to a deleted namespace.

```unison:hide
deleted.x = 1
stuff.thing = 2
```

```ucm:hide
scratch/main> add
scratch/main> delete.namespace deleted
```

## fork

I should be allowed to fork over a deleted namespace

```ucm
scratch/main> fork stuff deleted
```

The history from the `deleted` namespace should have been overwritten by the history from `stuff`.

```ucm
scratch/main> history stuff
scratch/main> history deleted
```

## move.namespace

```unison:hide
moveoverme.x = 1
moveme.y = 2
```

```ucm:hide
scratch/main> add
```

I should be able to move a namespace over-top of a deleted namespace.
The history should be that of the moved namespace.

```ucm
scratch/main> delete.namespace moveoverme
scratch/main> history moveme
scratch/main> move.namespace moveme moveoverme
scratch/main> history moveoverme
```
