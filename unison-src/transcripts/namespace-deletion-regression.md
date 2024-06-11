# Namespace deletion regression test

See https://github.com/unisonweb/unison/issues/1552

If branch operations aren't performed in the correct order it's possible to end up with unexpected results.

Previously the following sequence delete the current namespace
unexpectedly 😬.

```ucm
scratch/main> alias.term ##Nat.+ .Nat.+
scratch/main> ls Nat
scratch/main> move.namespace Nat Nat.operators
scratch/main> ls Nat
scratch/main> ls Nat.operators
```
