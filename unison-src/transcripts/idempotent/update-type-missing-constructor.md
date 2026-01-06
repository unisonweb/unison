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

> delete.term.force Foo.Bar

  I deleted these terms:

    1. Foo.Bar

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

Now we've set up a situation where the original constructor missing.

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
> view Foo

  type Foo = #5mod0n8ps2#0 Nat

> update

  Sorry, I wasn't able to perform the `update`:

  The type Foo has some constructors with missing names, and I
  can't `update` in this situation.

  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each unnamed constructor, and then try `update` again.
```
