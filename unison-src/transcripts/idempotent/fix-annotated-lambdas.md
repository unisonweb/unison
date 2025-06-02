``` ucm :hide
scratch/main> builtins.merge
```

Tests an erroneous lambda floating case involving annotations.

``` unison
foo : a -> a
foo x =
  bar ((f -> f x) : forall r. (a -> r) -> r)

bar : (forall r. (a -> r) -> r) -> a
bar k = k (x -> x)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      bar : (∀ r. (a -> r) ->{g} r) ->{g} a
      foo : a -> a
```

``` ucm
scratch/main> display foo

  x -> bar (f -> f x)
```
