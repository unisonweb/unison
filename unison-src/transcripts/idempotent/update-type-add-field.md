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
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view Foo

  type Foo = Bar Nat Nat

> find.verbose

  1. -- #hlhjq1lf1cvfevkvb9d441kkubn0f6s43gvrd4gcff0r739vomehjnov4b3qe8506fb5bm8m5ba0sol9mbljgkk3gb2qt2u02v6i2vo
     type Foo
     
  2. -- #hlhjq1lf1cvfevkvb9d441kkubn0f6s43gvrd4gcff0r739vomehjnov4b3qe8506fb5bm8m5ba0sol9mbljgkk3gb2qt2u02v6i2vo#0
     Foo.Bar : Nat -> Nat -> Foo
     
```
