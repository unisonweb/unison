# Meta.load reads a term's source from the codebase

`Meta.load : Link.Term -> {IO} Optional (meta.Term meta.TermF)` resolves
a term link by hash, walks back through the codebase's `CodeLookup`, and
returns the source AST as a `meta.Term`. This is the read side of the
eventual load/rewrite/store workflow.

``` ucm :hide
scratch/main> builtins.mergeio
```

Define a user term that itself references another user term.

``` unison
helper : Nat -> Nat
helper n = n + 1

usesHelper : Nat -> Nat
usesHelper n = helper n + helper n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + helper     : Nat -> Nat
  + usesHelper : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

Load `usesHelper` from the codebase, then hand the returned `meta.Term`
to `Meta.typecheck`. The loaded term has both the codebase's type
annotation (`: Nat -> Nat`) and a `Ref` to `helper`, so this exercises
both the `Ann` decoder in `MetaCompile` and the codebase-aware
`TypeLookup` from `Meta.typecheck`.

``` unison
loadAndCheck : '{IO} Either Text Text
loadAndCheck _ =
  match Meta.load (termLink usesHelper) with
    None -> Left "Meta.load returned None"
    Some metaTerm -> match Meta.typecheck metaTerm with
      Left e  -> Left ("typecheck failed: " ++ e)
      Right _ -> Right "loaded and typechecked"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + loadAndCheck : '{IO} Either Text Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run loadAndCheck

  Right "loaded and typechecked"
```
