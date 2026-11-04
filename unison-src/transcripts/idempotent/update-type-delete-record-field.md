``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat, baz : Int }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + Foo.bar        : Foo -> Nat
  + Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  + Foo.bar.set    : Nat -> Foo -> Foo
  + Foo.baz        : Foo -> Int
  + Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
  + Foo.baz.set    : Int -> Foo -> Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> find

  1. type Foo
  2. Foo.bar : Foo -> Nat
  3. Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  4. Foo.bar.set : Nat -> Foo -> Foo
  5. Foo.baz : Foo -> Int
  6. Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
  7. Foo.baz.set : Int -> Foo -> Foo
  8. Foo.Foo : Nat -> Int -> Foo
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ Foo.bar        : Foo -> Nat
  ~ Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  ~ Foo.bar.set    : Nat -> Foo -> Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

We want the field accessors to go away; but for now they are here, causing the update to fail.

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  I couldn't complete the update, because some existing
  definitions would no longer typecheck.

  You're now on a temporary branch, update-main, and the
  definitions to fix are in update-main-9e2e63bd.u.

  Edit the file, then run:

      scratch/update-main> load update-main-9e2e63bd.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the update instead, run `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm update-main-9e2e63bd.u
type Foo = { bar : Nat }

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

Foo.baz : Foo -> Int
Foo.baz = cases Foo _ baz -> baz

Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
Foo.baz.modify f = cases Foo bar baz -> Foo bar (f baz)

Foo.baz.set : Int -> Foo -> Foo
Foo.baz.set baz1 = cases Foo bar _ -> Foo bar baz1

```

The definitions related to the update are only present in the scratch file now,
and not in the temporary branch:

``` ucm
scratch/update-main> ls

  1. lib. (780 terms, 120 types)
```

so we can remove the unwanted definitions from the scratch file and `update` again to delete them:

``` unison
type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ Foo.bar        : Foo -> Nat
  ~ Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  ~ Foo.bar.set    : Nat -> Foo -> Foo
  - Foo.baz        : Foo -> Int
  - Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
  - Foo.baz.set    : Int -> Foo -> Foo

  ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/update-main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  I fast-forward merged scratch/update-main into scratch/main.

  Done.

scratch/main> find

  1. type Foo
  2. Foo.bar : Foo -> Nat
  3. Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  4. Foo.bar.set : Nat -> Foo -> Foo
  5. Foo.Foo : Nat -> Foo
```
