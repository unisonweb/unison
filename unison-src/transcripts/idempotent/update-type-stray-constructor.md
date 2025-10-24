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

> move.term Foo.Bar Stray.Bar

  Done.
```

Now we've set up a situation where the constructor is not where it's supposed to be; it's somewhere else.

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Note that the constructor name shown here (implied to be called `Foo.Stray.Bar`) doesn't really exist, it's just showing up due to a pretty-printer bug.

``` ucm :error
> view Foo

  type Foo = Stray.Bar Nat

> update

  Sorry, I wasn't able to perform the update:

  The type Foo has some constructors with missing names, and I
  can't perform an update in this situation.

  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each unnamed constructor, and then try updating again.
```
