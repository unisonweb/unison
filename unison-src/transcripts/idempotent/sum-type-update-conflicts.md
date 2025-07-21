# Regression test for updates which conflict with an existing data constructor

https://github.com/unisonweb/unison/issues/2786

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

First we add a sum-type to the codebase.

``` unison
structural type X = x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type X

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now we update the type, changing the name of the constructors, *but*, we simultaneously
add a new top-level term with the same name as the old constructor.

``` unison
structural type X = y | z

X.x : Text
X.x = "some text that's not in the codebase"

dependsOnX = Text.size X.x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ structural type X

  + dependsOnX : Nat
  ~ X.x : Text
      (was also named lib.builtins.Unit.Unit)

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

This update should succeed since the conflicted constructor
is removed in the same update that the new term is being added.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
