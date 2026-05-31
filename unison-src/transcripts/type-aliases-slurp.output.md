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

  + f : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

The expected output: a line like `+ type alias Endo a = a -> a` appears in
the slurp output above (or near) the `+ f : Nat -> Nat` line.

Currently the slurp pipeline only enumerates data and effect decls, so the
alias is silently consumed and the output only mentions `f`.

``` ucm
> add

  Done.
```
