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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      fib : Nat -> Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    fib : Nat -> Nat

.> view fib

  fib : Nat -> Nat
  fib n =
    case n of
      0 -> 0
      n ->
        [: run time O(n) - note that O(log n) is possible :]
        fib (Nat.drop n 1)

```
Note that @ and :] must be escaped within docs.

```unison
escaping = [: Docs look [: like \@this \:] :]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      escaping : Doc
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    escaping : Doc

.> view escaping

  escaping : Doc
  escaping = [: Docs look [: like \@this \:] :]

```
-- TODO - consider the following...

[: hi :]

[: hi
   there :]

[:
   hi
   there
:]