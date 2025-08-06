``` ucm
> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
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
foo : Nat
foo = 6
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Nat

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

> view bar

  bar : Nat
  bar =
    use Nat +
    foo + 10
```
