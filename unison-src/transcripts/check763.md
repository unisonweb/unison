Regression test for https://github.com/unisonweb/unison/issues/763

```ucm:hide
.> builtins.merge
```

```unison
(+-+) : Nat -> Nat -> Nat
(+-+) x y = x * y
```

```ucm
.> add
.> move.term +-+ boppitybeep
.> move.term boppitybeep +-+
```

