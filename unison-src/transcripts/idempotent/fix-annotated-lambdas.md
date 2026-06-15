``` ucm :hide
> builtins.merge
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

  + bar : (∀ r. (a -> r) -> r) -> a
  + foo : a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm
> display foo

  x -> bar (f -> f x)
```
