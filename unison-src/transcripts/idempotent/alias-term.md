`alias.term` makes a new name for a term.

``` ucm :hide
> builtins.mergeio lib.builtins
```

``` ucm
> alias.term lib.builtins.bug foo

  Done.

> ls .

  1. foo  (a -> b)
  2. lib/ (755 terms, 118 types)
```

It won't create a conflicted name, though.

``` ucm :error
> alias.term lib.builtins.todo foo

  ⚠️

  A term by that name already exists.
```

``` ucm
> ls .

  1. foo  (a -> b)
  2. lib/ (755 terms, 118 types)
```

You can use `debug.alias.term.force` for that.

``` ucm
> debug.alias.term.force lib.builtins.todo foo

  Done.

> ls .

  1. foo  (a -> b)
  2. foo  (a -> b)
  3. lib/ (755 terms, 118 types)
```
