# Namespace deletion regression test

See https://github.com/unisonweb/unison/issues/1552

If branch operations aren't performed in the correct order it's possible to end up with unexpected results

```ucm
.> alias.term ##Nat.+ .Nat.+
.> ls
.> move.namespace Nat Nat.operators
.> ls
```
