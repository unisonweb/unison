# Type aliases

``` ucm :hide
> builtins.mergeio
```

A `type alias` declaration introduces a name for a parameterized type
expression. Aliases are transparent: every use site is fully expanded against
the body before hashing, so `f : Endo Nat -> Nat` hashes identically to
`f : (Nat -> Nat) -> Nat`.

``` unison
type alias Endo a = a -> a

f : Endo Nat
f x = x +  1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  + f : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

Adding the alias and the term:

``` ucm
> add

  Done.
```

The alias is now persisted in the codebase. It appears alongside `f` in
the namespace; `view`-level integration (looking up an alias by name and
rendering it back as a `type alias` declaration) is a follow-up.
