Docs can be used as inline code comments.

```unison
fib : Nat -> Nat
fib n =
  case n of
    0 -> 0
    n ->
      [: run time O(n) - note that O(log n) is possible :]
      fib (Nat.drop n 1)
```

```ucm
.> add
.> view fib
```

Note that @ and :] must be escaped within docs.

```unison
escaping = [: Docs look [: like \@this \:] :]
```

```ucm
.> add
.> view escaping
```

-- TODO - consider the following...

[: hi :]

[: hi
   there :]

[:
   hi
   there
:]