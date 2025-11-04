``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> alias.term Foo.Bar Stray.BarAlias

  Done.
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> update

  Sorry, I wasn't able to perform the `update`, because I need
  all constructor names to be nested somewhere beneath the
  corresponding type name.

  The constructor Stray.BarAlias is not nested beneath the
  corresponding type name. Please either use `move` to move it,
  or if it's an extra copy, you can simply `delete.force` it.
  Then try `update` again.
```
