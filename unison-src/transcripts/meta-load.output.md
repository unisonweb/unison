# Meta.load reads a term's source from the codebase

`Meta.load : Link.Term -> {IO} Optional (meta.Term meta.TermF)` resolves
a term link by hash, walks back through the codebase's `CodeLookup`, and
returns the source AST as a `meta.Term`. This is the read side of the
load-rewrite-store workflow.

``` ucm :hide
scratch/main> builtins.mergeio
```

Define an ordinary user term.

``` unison
helper : Nat -> Nat
helper n = n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + helper : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

`Meta.load` of a user-defined term returns `Some`. The runtime backmaps
the runtime reference through `EvalCtx`'s float/intermediate remaps to
recover the codebase Reference.Id before consulting the `CodeLookup`.

``` unison
isSome : '{IO} Boolean
isSome _ = match Meta.load (termLink helper) with
  None -> false
  Some _ -> true
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + isSome : '{IO} Boolean

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run isSome

  true
```
