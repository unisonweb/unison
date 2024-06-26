# Behaviour of namespace histories during a merge.

Note: This is a descriptive test meant to capture the current behaviour of
branch histories during a merge.
It isn't prescriptive about how merges _should_ work with respect to child branches,
but I think we should at least notice if we change things by accident.


## Setting up some history

```ucm:hide
scratch/main> builtins.merge
```

```unison:hide
parent.top = "top"
parent.child.thing = "parent.child.thing"
```

The child branch has a single history node representing the addition of `parent.child.thing`.

```ucm
scratch/main> add
scratch/main> history parent.child
```

If we add another thing to the child namespace it should add another history node to both the child and parent.

```unison:hide
parent.child.thing2 = "parent.child.thing2"
```

```ucm
scratch/main> add
scratch/main> history parent
scratch/main> history parent.child
```

## Forking off some history on a separate branch

Now we fork the parent namespace to make some changes.

```ucm
scratch/main> fork parent parent_fork
```

```unison:hide
parent_fork.child.thing3 = "parent_fork.child.thing3"
```

The child should have a new history node after adding `thing3`

```ucm
scratch/main> add
scratch/main> history parent_fork.child
```

## Saving our parent state

Split off two separate forks, one for testing squash merges, one for standard merges.

```ucm:hide
scratch/main> fork parent parent_squash_base
scratch/main> fork parent parent_merge_base
```

## Squash merge

For a squash merge, when I squash-merge back into parent, we expect `parent_fork.child.thing3` to be added.

```ucm
scratch/main> merge.old.squash parent_fork parent_squash_base
scratch/main> history parent_squash_base
```

Notice that with the current behaviour, the history of `parent.child` is completely wiped out, containing nothing from the source OR destination.

```ucm
scratch/main> history parent.child
scratch/main> history parent_fork.child
scratch/main> history parent_squash_base.child
```

## Standard merge

For a standard merge, if I merge back into parent, we expect `parent_fork.child.thing3` to be added.

```ucm
scratch/main> merge.old parent_fork parent_merge_base
scratch/main> history parent_merge_base
```

Child histories should also be *merged*.

```ucm
scratch/main> history parent.child
scratch/main> history parent_fork.child
scratch/main> history parent_merge_base.child
```
