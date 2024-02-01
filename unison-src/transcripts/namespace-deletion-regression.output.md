# Namespace deletion regression test

See https://github.com/unisonweb/unison/issues/1552

If branch operations aren't performed in the correct order it's possible to end up with unexpected results.

Previously the following sequence delete the current namespace
unexpectedly 😬.

```ucm
.> alias.term ##Nat.+ .Nat.+

  Done.

.> ls Nat

  1. + (##Nat -> ##Nat -> ##Nat)

.> move.namespace Nat Nat.operators

  Done.

.> ls Nat

  1. operators/ (1 term)

.> ls Nat.operators

  1. + (##Nat -> ##Nat -> ##Nat)

```
