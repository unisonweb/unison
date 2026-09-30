# Type aliases

``` ucm :hide
> builtins.mergeio
```

A `type alias` declaration introduces a name for a parameterized type
expression. Alias refs are stored in types verbatim and expanded lazily by
the typechecker, so `f : Endo Nat` and `g : Nat -> Nat` are distinct
definitions with distinct hashes even when their bodies coincide.

``` unison
type alias Endo a = a -> a

f : Endo Nat
f x = x +  1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  + f : Endo Nat

  Run `update` to apply these changes to your codebase.
```

Adding the alias and the term:

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

The alias is persisted alongside `f`, and `f`'s type still mentions
`Endo Nat` — no expansion happens at the storage layer.
