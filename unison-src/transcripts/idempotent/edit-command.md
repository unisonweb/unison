``` ucm :hide
> builtins.merge
```

``` unison
foo = 123

bar = 456

mytest = [Ok "ok"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar    : Nat
  + foo    : Nat
  + mytest : [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> edit.new foo bar

> edit.new mytest
```

``` unison :added-by-ucm scratch.u
bar : Nat
bar = 456

foo : Nat
foo = 123
```

``` unison :added-by-ucm scratch.u
test> mytest = [Ok "ok"]
```

``` ucm :error
> edit.new missing

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    missing
```

``` ucm :hide
> project.delete scratch
```

# `edit`

The `edit` command adds to the current fold, and takes care not to add definitions that are already in the file.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

This stanza does nothing for some reason (transcript runner bug?), so we repeat it twice.

``` unison
foo = 17
bar = 18
baz = 19
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + baz : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` unison
foo = 17
bar = 18
baz = 19
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + baz : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = 17
bar = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

``` ucm
> edit bar baz
```

``` unison :added-by-ucm scratch.u
baz : Nat
baz = 19
```

``` ucm :hide
> project.delete scratch
```
