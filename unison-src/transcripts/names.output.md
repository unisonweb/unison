# `names` command

Example uses of the `names` command and output

```unison
-- Some names with the same value
some.place.x = 1
some.otherplace.y = 1
some.otherplace.x = 10
somewhere.z = 1
-- Some similar name with a different value
somewhere.y = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      some.otherplace.x : ##Nat
      some.otherplace.y : ##Nat
      some.place.x      : ##Nat
      somewhere.y       : ##Nat
      somewhere.z       : ##Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:

    some.otherplace.x : ##Nat
    some.otherplace.y : ##Nat
    some.place.x      : ##Nat
    somewhere.y       : ##Nat
    somewhere.z       : ##Nat

```
`names` searches relative to the current path.

```ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.
-- But we don't see somewhere.z which is has the same value but is out of our namespace
  ☝️  The namespace .some is empty.

scratch/main some> names x

  😶

  I couldn't find anything by that name.

  Tip: Use `names.global` to see more results.

```

```ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.-- But we don't see somewhere.z which is has the same value but is out of our namespace.some> names x-- We can search by hash, and see all aliases of that hash.some> names #gjmq673r1v-- If the query is absolute, treat it as a `names.global`.some> names .some.place.x
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  😶

  I couldn't find anything by that name.

  Tip: Use `names.global` to see more results.

