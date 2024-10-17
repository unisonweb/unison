``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm :added-by-ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
scratch/main> alias.term Foo.Bar Stray.BarAlias

  Done.
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
```

``` ucm :error
scratch/main> update

  Sorry, I wasn't able to perform the update, because I need all
  constructor names to be nested somewhere beneath the
  corresponding type name.

  The constructor Stray.BarAlias is not nested beneath the
  corresponding type name. Please either use `move` to move it,
  or if it's an extra copy, you can simply `delete` it. Then try
  the update again.
```
