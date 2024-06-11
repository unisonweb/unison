# Namespace deletion regression test

See https://github.com/unisonweb/unison/issues/1552

If branch operations aren't performed in the correct order it's possible to end up with unexpected results.

Previously the following sequence delete the current namespace
unexpectedly 😬.

```ucm
scratch/main> alias.term ##Nat.+ .Nat.+

  Done.

scratch/main> ls Nat

  nothing to show

```

```ucm
scratch/main> alias.term ##Nat.+ .Nat.+scratch/main> ls Natscratch/main> move.namespace Nat Nat.operatorsscratch/main> ls Natscratch/main> ls Nat.operators
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  nothing to show

