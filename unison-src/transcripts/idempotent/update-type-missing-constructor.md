``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      type Foo
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> delete.term Foo.Bar

  Done.
```

Now we've set up a situation where the original constructor missing.

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
```

``` ucm :error
scratch/main> view Foo

  type Foo = #5mod0n8ps2#0 Nat

scratch/main> update

  Sorry, I wasn't able to perform the update:

  The type Foo has some constructors with missing names, and I
  can't perform an update in this situation.

  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each unnamed constructor, and then try the update again.
```
