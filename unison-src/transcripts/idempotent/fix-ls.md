``` ucm
test-ls/main> builtins.merge

  Done.
```

``` unison
foo.bar.add x y = x Int.+ y

foo.bar.subtract x y = x Int.- y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      foo.bar.add      : Int -> Int -> Int
      foo.bar.subtract : Int -> Int -> Int
```

``` ucm
test-ls/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

test-ls/main> ls foo

  1. bar/ (2 terms)

test-ls/main> ls 1

  1. add      (Int -> Int -> Int)
  2. subtract (Int -> Int -> Int)
```
