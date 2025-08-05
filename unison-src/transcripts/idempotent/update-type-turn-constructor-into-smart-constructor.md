``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + makeFoo : Nat -> Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo = internal.Bar Nat

Foo.Bar : Nat -> Foo
Foo.Bar n = internal.Bar n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ Foo.Bar : Nat -> Foo

  (and 1 unchanged type)

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

> view Foo

  type Foo = internal.Bar Nat

> find.verbose

  1. -- #oebc8v8v9lob5bnq7go1pjhfjbtnh8dmfhontua90t3mji0cl91t1dqaece9quofrk1vsbq6g0ukfigoi0vmvc01v8roceppejlgbs8
     type Foo
     
  2. -- #gl18p1lnbeari67ohdt9n46usnvsl59a6up1lhd9r808pqb7tt5edsf65o98bqcvb529mfm7q631ciuv2t5nqnde1i7b9t5mlu1drto
     Foo.Bar : Nat -> Foo
     
  3. -- #oebc8v8v9lob5bnq7go1pjhfjbtnh8dmfhontua90t3mji0cl91t1dqaece9quofrk1vsbq6g0ukfigoi0vmvc01v8roceppejlgbs8#0
     Foo.internal.Bar : Nat -> Foo
     
  4. -- #td96hudai64mf0qgtusc70ehv98krs10jghdipjluc6cp4j8ac65msrt3tji18enpm2tm8d8h2qcf3parke19g7s17ipkd925m3061g
     makeFoo : Nat -> Foo
     
```
