``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type A
  + structural type A.B
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

``` ucm :error
> update

  The type A.B is an alias of A. I'm not able to perform an
  update when a type exists nested under an alias of itself.
  Please separate them or delete one copy, and then try updating
  again.
```
