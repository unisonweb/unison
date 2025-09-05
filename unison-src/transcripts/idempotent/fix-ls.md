``` ucm
> builtins.merge

  Done.
```

``` unison
foo.bar.add x y = x Int.+ y

foo.bar.subtract x y = x Int.- y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo.bar.add      : Int -> Int -> Int
  + foo.bar.subtract : Int -> Int -> Int

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> ls foo

  1. bar. (2 terms)

> ls 1

  1. add      (Int -> Int -> Int)
  2. subtract (Int -> Int -> Int)
```
