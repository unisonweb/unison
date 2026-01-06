`alias.type` makes a new name for a type.

``` ucm :hide
> builtins.mergeio lib.builtins
```

``` ucm
> alias.type lib.builtins.Nat Foo

  Done.

> ls .

  1. Foo  (builtin type)
  2. lib. (855 terms, 125 types)
```

It won't create a conflicted name, though.

``` ucm :error
> alias.type lib.builtins.Int Foo

  ⚠️

  A type by that name already exists.
```

``` ucm
> ls .

  1. Foo  (builtin type)
  2. lib. (855 terms, 125 types)
```

You can use `debug.alias.type.force` for that.

``` ucm
> debug.alias.type.force lib.builtins.Int Foo

  Done.

> ls .

  1. Foo  (builtin type)
  2. Foo  (builtin type)
  3. lib. (855 terms, 125 types)
```
