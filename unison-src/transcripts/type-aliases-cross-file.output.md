# Cross-file type alias resolution

Confirms that a `type alias` declared in one file can be referenced from a
later, separate scratch file.

``` ucm :hide
> builtins.mergeio
```

First file: declare the alias and add it to the codebase.

``` unison
type alias Endo a = a -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Second file: reference the alias from a fresh scratch file. `g`'s stored
type signature keeps `Endo Nat` intact, and `view g` renders it back the
same way.

``` unison
g : Endo Nat
g x = x + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + g : Endo Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view g

  g : Endo Nat
  g x =
    use Nat +
    x + 2
```
