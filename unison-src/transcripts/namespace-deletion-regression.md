# Namespace deletion regression test

See https://github.com/unisonweb/unison/issues/1552

If branch operations aren't performed in the correct order it's possible to end up with unexpected results.

Previously the following sequence delete the current namespace
unexpectedly 😬.

```ucm
.> alias.term ##Nat.+ .Nat.+
.> ls Nat
.> move.namespace Nat Nat.operators
.> ls Nat
.> ls Nat.operators
```
