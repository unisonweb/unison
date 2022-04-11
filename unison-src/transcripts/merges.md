# Forking and merging namespaces in `ucm`

```ucm:hide
.master> builtins.merge
.> cd .
```

The Unison namespace is a versioned tree of names that map to Unison definitions. You can change this namespace and fork and merge subtrees of it. Let's start by introducing a few definitions into a new namespace, `foo`:

```unison
x = 42
```

```ucm
.> add
```

Let's move `x` into a new namespace, `master`:

```ucm
.> rename.term x master.x
```

If you want to do some experimental work in a namespace without disturbing anyone else, you can `fork` it (which is a shorthand for `copy.namespace`). This creates a copy of it, preserving its history.

> __Note:__ these copies are very efficient to create as they just have pointers into the same underlying definitions. Create as many as you like.

Let's go ahead and do this:

```
.> fork master feature1
.> view master.x
.> view feature1.x
```

Great! We can now do some further work in the `feature1` branch, then merge it back into `master` when we're ready.

```unison
y = "hello"
```

```ucm
.feature1> add
.master> merge .feature1
.master> view y
```

> Note: `merge src`, with one argument, merges `src` into the current namespace. You can also do `merge src dest` to merge into any destination namespace.

Notice that `master` now has the definition of `y` we wrote.

We can also delete the fork if we're done with it. (Don't worry, it's still in the `history` and can be resurrected at any time.)

```ucm
.> delete.namespace .feature1
.> history .feature1
.> history
```

To resurrect an old version of a namespace, you can learn its hash via the `history` command, then use `fork #namespacehash .newname`.

## Concurrent edits and merges

In the above scenario the destination namespace (`master`) was strictly behind the source namespace, so the merge didn't have anything interesting to do (Git would call this a "fast forward" merge). In other cases, the source and destination namespaces will each have changes the other doesn't know about, and the merge needs to something more interesting. That's okay too, and Unison will merge those results, using a 3-way merge algorithm.

> __Note:__ When merging nested namespaces, Unison actually uses a recursive 3-way merge, so it finds a different (and possibly closer) common ancestor at each level of the tree.

Let's see how this works. We are going to create a copy of `master`, add and delete some definitions in `master` and in the fork, then merge.

```ucm
.> fork master feature2
```

Here's one fork, we add `z` and delete `x`:

```unison
z = 99
```

```ucm
.feature2> add
.feature2> delete.term x
```

And here's the other fork, where we update `y` and add a new definition, `frobnicate`:

```unison
master.y = "updated y"
master.frobnicate n = n + 1
```

```ucm
.> update
.> view master.y
.> view master.frobnicate
```

At this point, `master` and `feature2` both have some changes the other doesn't know about. Let's merge them.

```ucm
.> merge feature2 master
```

Notice that `x` is deleted in the merged branch (it was deleted in `feature2` and untouched by `master`):

```ucm:error
.> view master.x
```

And notice that `y` has the most recent value, and that `z` and `frobnicate` both exist as well:

```ucm
.> view master.y
.> view master.z
.> view master.frobnicate
```

## FAQ

* What happens if namespace1 deletes a name that namespace2 has updated? A: ???
* ...
