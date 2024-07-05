
A short script to test mutable references with local scope.

```ucm:hide
scratch/main> builtins.merge
```

```unison
test = Scope.run 'let
  r = Scope.ref 0
  Ref.write r 1
  i = Ref.read r
  Ref.write r 2
  j = Ref.read r
  Ref.write r 5
  (i, j, Ref.read r)

> test
```
