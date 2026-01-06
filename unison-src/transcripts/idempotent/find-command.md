``` ucm :hide
> builtins.merge lib.builtin
```

``` unison :hide
foo = 1
libfoo = 2
libbar = 3
cat.foo = 4
cat.lib.foo = 5
cat.lib.bar = 6
somewhere.bar = 7
```

``` ucm :hide
> update

> move libfoo lib.foo

> move libbar lib.bar
```

``` ucm
> find foo

  1. cat.foo : Nat
  2. foo : Nat

> view 1

  cat.foo : Nat
  cat.foo = 4

> find.all foo

  1. cat.foo : Nat
  2. cat.lib.foo : Nat
  3. lib.foo : Nat
  4. foo : Nat

> view 1

  cat.foo : Nat
  cat.foo = 4
```

``` ucm
> find-in cat foo

  1. foo : Nat

> view 1

  cat.foo : Nat
  cat.foo = 4

> find-in.all cat foo

  1. lib.foo : Nat
  2. foo : Nat

> view 1

  cat.lib.foo : Nat
  cat.lib.foo = 5
```

Finding within a namespace

``` ucm
> find bar

  1. somewhere.bar : Nat

> debug.find.global bar

  Found results in scratch/main

  1. .cat.lib.bar : Nat
  2. .lib.bar : Nat
  3. .somewhere.bar : Nat

> find-in somewhere bar

  1. bar : Nat
```

``` ucm :error
> find baz

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  😶

  No results. Check your spelling, or try using tab completion
  to supply command arguments.

  `debug.find.global` can be used to search outside the current
  namespace.
```
