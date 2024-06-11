
# Squash merges

`squash src dest` merges can be used to merge from `src` to `dest`, discarding the history of `src`. It's useful when the source namespace history is irrelevant or has a bunch of churn you wish to discard. Often when merging small pull requests, you'll use a squash merge.

Let's look at some examples. We'll start with a namespace with just the builtins. Let's take a look at the hash of this namespace:

```ucm
scratch/main> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #i3vp9o9btm (start of history)

scratch/main> fork builtin builtin2

  Done.

```
(We make a copy of `builtin` for use later in this transcript.)

Now suppose we `fork` a copy of builtin, then rename `Nat.+` to `frobnicate`, then rename it back. Notice this produces multiple entries in the history:

```ucm
scratch/main> fork builtin mybuiltin

  Done.

  ☝️  The namespace .mybuiltin is empty.

.mybuiltin> rename.term Nat.+ Nat.frobnicate

  ⚠️
  
  I don't know about that term.

```

```ucm
scratch/main> fork builtin mybuiltin.mybuiltin> rename.term Nat.+ Nat.frobnicate.mybuiltin> rename.term Nat.frobnicate Nat.+.mybuiltin> history
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  I don't know about that term.

