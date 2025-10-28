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

> alias.term Foo.Bar Foo.BarAlias

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

  Sorry, I wasn't able to perform the `update`:

  The type Foo has a constructor with multiple names, and I
  can't `update` in this situation:

    * Foo.Bar
    * Foo.BarAlias

  Please `delete.force` all but one name for each constructor,
  and then try `update` again.
```
