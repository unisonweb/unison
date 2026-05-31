# Aliases in slurp output

Confirms that the "Loading changes detected" output that UCM shows after
typechecking a file lists `type alias` declarations alongside data decls,
effects, and terms.

``` ucm :hide
> builtins.mergeio
```

``` unison
type alias Endo a = a -> a

f : Endo Nat
f x = x + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  + f : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

The alias appears as `+ type alias Endo a = a -> a` alongside the term.

``` ucm
> add

  Done.
```
