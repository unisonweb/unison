# `edit.dependents`

The `edit.dependents` command is like `edit`, but it adds a definition and all of its transitive dependents to the file
(being careful not to add anything that's already there).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
type Foo = Foo Nat Nat
type Bar = { bar : Foo }

baz : Bar -> Bar
baz x = x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Bar
  + type Foo

  + Bar.bar        : Bar -> Foo
  + Bar.bar.modify : (Foo ->{g} Foo) -> Bar ->{g} Bar
  + Bar.bar.set    : Foo -> Bar -> Bar
  + baz            : Bar -> Bar

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Let's populate our scratch file with `Bar` (and its auto-generated accessors), then `edit.dependents` its dependency
`Foo`, which should add `Foo` and `baz`.

``` unison
type Bar = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Bar

  ~ Bar.bar        : Bar -> Nat
  ~ Bar.bar.modify : (Nat ->{g} Nat) -> Bar ->{g} Bar
  ~ Bar.bar.set    : Nat -> Bar -> Bar

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> edit.dependents Foo

  Loading branch...

  Identifying dependents...

  Loading dependents...

  ☝️

  I added 2 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
type Foo = Foo Nat Nat

baz : Bar -> Bar
baz x = x
```

``` ucm :hide
scratch/main> project.delete scratch
```
