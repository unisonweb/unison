
```ucm:hide
scratch/main> builtins.merge
```

# Squash merges

`squash src dest` merges can be used to merge from `src` to `dest`, discarding the history of `src`. It's useful when the source namespace history is irrelevant or has a bunch of churn you wish to discard. Often when merging small pull requests, you'll use a squash merge.

Let's look at some examples. We'll start with a namespace with just the builtins. Let's take a look at the hash of this namespace:

```ucm
scratch/main> history builtin
scratch/main> fork builtin builtin2
```

(We make a copy of `builtin` for use later in this transcript.)

Now suppose we `fork` a copy of builtin, then rename `Nat.+` to `frobnicate`, then rename it back. Notice this produces multiple entries in the history:

```ucm
scratch/main> fork builtin mybuiltin
.mybuiltin> rename.term Nat.+ Nat.frobnicate
.mybuiltin> rename.term Nat.frobnicate Nat.+
.mybuiltin> history
```

If we merge that back into `builtin`, we get that same chain of history:

```ucm
scratch/main> merge.old mybuiltin builtin
scratch/main> history builtin
```

Let's try again, but using a `merge.squash` (or just `squash`) instead. The history will be unchanged:

```ucm
scratch/main> merge.old.squash mybuiltin builtin2
scratch/main> history builtin2
```

The churn that happened in `mybuiltin` namespace ended up back in the same spot, so the squash merge of that namespace with our original namespace had no effect.

## Another example

Let's look at a more interesting example, where the two namespaces have diverged a bit. Here's our starting namespace:

```unison:hide
x = 1
```

```ucm
.trunk> add
scratch/main> fork trunk alice
scratch/main> fork trunk bob
```

Alice now does some hacking:

```unison:hide
radNumber = 348
bodaciousNumero = 2394
neatoFun x = x
```

```ucm
.alice> add
.alice> rename.term radNumber superRadNumber
.alice> rename.term neatoFun productionReadyId
```

Meanwhile, Bob does his own hacking:

```unison:hide
whatIsLove = "?"
babyDon'tHurtMe = ".. Don't hurt me..."
no more = no more
```

```ucm
.bob> add
```

At this point, Alice and Bob both have some history beyond what's in trunk:

```ucm
scratch/main> history trunk
scratch/main> history alice
scratch/main> history bob
```

Alice then squash merges into `trunk`, as does Bob. It's as if Alice and Bob both made their changes in one single commit.

```ucm
scratch/main> merge.old.squash alice trunk
scratch/main> history trunk
scratch/main> merge.old.squash bob trunk
scratch/main> history trunk
```

Since squash merges don't produce any merge nodes, we can `undo` a couple times to get back to our starting state:

```ucm
scratch/main> undo
scratch/main> undo
scratch/main> history trunk
```

This time, we'll first squash Alice and Bob's changes together before squashing their combined changes into `trunk`. The resulting `trunk` will have just a single entry in it, combining both Alice and Bob's changes:

```ucm
scratch/main> merge.old.squash alice bob
scratch/main> merge.old.squash bob trunk
scratch/main> history trunk
```

So, there you have it. With squashing, you can control the granularity of your history.

## Throwing out all history

Another thing we can do is `squash` into an empty namespace. This effectively makes a copy of the namespace, but without any of its history:

```ucm
scratch/main> merge.old.squash alice nohistoryalice
scratch/main> history nohistoryalice
```

There's nothing really special here, `squash src dest` discards `src` history that comes after the LCA of `src` and `dest`, it's just that in the case of an empty namespace, that LCA is the beginning of time (the empty namespace), so all the history of `src` is discarded.

## Checking for handling of deletes

This checks to see that squashing correctly preserves deletions:

```ucm
.delete> builtins.merge
.delete> fork builtin builtin2
.delete> delete.term.verbose builtin2.Nat.+
.delete> delete.term.verbose builtin2.Nat.*
.delete> merge.old.squash builtin2 builtin
.delete> history builtin
```

Notice that `Nat.+` and `Nat.*` are deleted by the squash, and we see them deleted in one atomic step in the history.

Just confirming that those two definitions are in fact removed:

```ucm:error
.delete> view .delete.builtin.Nat.+
```

```ucm:error
.delete> view .delete.builtin.Nat.*
```

## Caveats

If you `squash mystuff trunk`, you're discarding any history of `mystuff` and just cons'ing onto the history of `trunk`. Thus, don't expect to be able to `merge trunk mystuff` later and get great results. Squashing should only be used when you don't care about the history (and you know others haven't pulled and built on your line of history being discarded, so they don't care about the history either).
