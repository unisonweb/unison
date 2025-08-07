``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + Foo.bar        : Foo -> Nat
  + Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
  + Foo.bar.set    : Nat -> Foo -> Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
