# Forking and merging namespaces in `ucm`

The Unison namespace is a versioned tree of names that map to Unison definitions. You can change this namespace and fork and merge subtrees of it. Let's start by introducing a few definitions into a new namespace, `foo`:

```unison
x = 42
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    x : ##Nat

```
Let's move `x` into a new namespace, `master`:

```ucm
scratch/main> rename.term x master.x

  Done.

```
If you want to do some experimental work in a namespace without disturbing anyone else, you can `fork` it (which is a shorthand for `copy.namespace`). This creates a copy of it, preserving its history.

> __Note:__ these copies are very efficient to create as they just have pointers into the same underlying definitions. Create as many as you like.

Let's go ahead and do this:

```scratch
/main> fork master feature1
scratch/main> view master.x
scratch/main> view feature1.x

```

Great! We can now do some further work in the `feature1` branch, then merge it back into `master` when we're ready.

```unison
y = "hello"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : ##Text

```
```ucm
  ☝️  The namespace .feature1 is empty.

.feature1> add

  ⍟ I've added these definitions:
  
    y : ##Text

.master> merge.old .feature1

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. y : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

.master> view y

  y : Text
  y = "hello"

```
> Note: `merge src`, with one argument, merges `src` into the current namespace. You can also do `merge src dest` to merge into any destination namespace.

Notice that `master` now has the definition of `y` we wrote.

We can also delete the fork if we're done with it. (Don't worry, even though the history at that path is now empty,
it's still in the `history` of the parent namespace and can be resurrected at any time.)

```ucm
scratch/main> delete.namespace feature1

  ⚠️
  
  The namespace feature1 doesn't exist.

```

```ucm
scratch/main> delete.namespace feature1scratch/main> history .feature1scratch/main> history
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The namespace feature1 doesn't exist.

