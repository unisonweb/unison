# A real rewriter: decompile → transform → store → eval

This transcript writes a small AST rewriter as ordinary Unison code,
applies it to a user-defined function, stores the rewritten form back
to the codebase, and confirms the new term evaluates to a different
result than the original.

The killer-workflow loop in pure Unison:

``` 
Meta.decompile  →  meta.Term meta.TermF
              ↓     (transform)
Meta.store      →  new Link.Term
Meta.eval       →  observable behavior change
```

``` ucm :hide
scratch/main> builtins.mergeio
```

Define a small ABT walker that applies a `TermF -> TermF` rewrite at
every internal node of a `meta.Term`. Three mutually-recursive
helpers — one each for `meta.Term`, `meta.ABT`, `meta.TermF`.

``` unison
mapTm : (meta.TermF (meta.Term meta.TermF) ->{} meta.TermF (meta.Term meta.TermF))
     -> meta.Term meta.TermF
     -> meta.Term meta.TermF
mapTm f tm = match tm with
  meta.Term.Term frees abt -> meta.Term.Term frees (mapTm.abt f abt)

mapTm.abt : (meta.TermF (meta.Term meta.TermF) ->{} meta.TermF (meta.Term meta.TermF))
         -> meta.ABT meta.TermF (meta.Term meta.TermF)
         -> meta.ABT meta.TermF (meta.Term meta.TermF)
mapTm.abt f abt = match abt with
  meta.ABT.Var n -> meta.ABT.Var n
  meta.ABT.Abs n body -> meta.ABT.Abs n (mapTm f body)
  meta.ABT.Cycle body -> meta.ABT.Cycle (mapTm f body)
  meta.ABT.Tm tf -> meta.ABT.Tm (f (mapTm.tf f tf))

mapTm.tf : (meta.TermF (meta.Term meta.TermF) ->{} meta.TermF (meta.Term meta.TermF))
        -> meta.TermF (meta.Term meta.TermF)
        -> meta.TermF (meta.Term meta.TermF)
mapTm.tf f tf = match tf with
  meta.TermF.App a b -> meta.TermF.App (mapTm f a) (mapTm f b)
  meta.TermF.Lam a -> meta.TermF.Lam (mapTm f a)
  meta.TermF.Let a b -> meta.TermF.Let (mapTm f a) (mapTm f b)
  meta.TermF.If c t e -> meta.TermF.If (mapTm f c) (mapTm f t) (mapTm f e)
  meta.TermF.Handle h e -> meta.TermF.Handle (mapTm f h) (mapTm f e)
  meta.TermF.Ann a ty -> meta.TermF.Ann (mapTm f a) ty
  other -> other
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mapTm     : (TermF (meta.Term TermF)
                 -> TermF (meta.Term TermF))
                -> meta.Term TermF
                -> meta.Term TermF
  + mapTm.abt : (TermF (meta.Term TermF)
                 -> TermF (meta.Term TermF))
                -> ABT TermF (meta.Term TermF)
                -> ABT TermF (meta.Term TermF)
  + mapTm.tf  : (TermF (meta.Term TermF)
                 -> TermF (meta.Term TermF))
                -> TermF (meta.Term TermF)
                -> TermF (meta.Term TermF)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

A trivial rewrite: scale every `Nat` literal by a constant.

``` unison
scaleNatLits : Nat -> meta.TermF (meta.Term meta.TermF) -> meta.TermF (meta.Term meta.TermF)
scaleNatLits k tf = match tf with
  meta.TermF.Lit (meta.Literal.LitNat n) ->
    meta.TermF.Lit (meta.Literal.LitNat (n Nat.* k))
  other -> other
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + scaleNatLits : Nat
                   -> TermF (meta.Term TermF)
                   -> TermF (meta.Term TermF)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

The function we'll rewrite. `helper 0 = 1`; after scaling every Nat
literal by `100`, the rewritten version should compute `helper 0 = 100`.

``` unison
helper : Nat -> Nat
helper n = n Nat.+ 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + helper : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

Run the workflow: decompile, transform, store, eval both, compare.

``` unison
rewriteAndCompare : '{IO} Either Text (Nat, Nat)
rewriteAndCompare _ =
  decompiled = Meta.decompile helper
  rewritten = mapTm (scaleNatLits 100) decompiled
  match Meta.store rewritten with
    Left e -> Left e
    Right newLink ->
      origFn = Meta.eval (termLink helper)
      newFn = Meta.eval newLink
      Right (origFn 0, newFn 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + rewriteAndCompare : '{IO} Either Text (Nat, Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run rewriteAndCompare

  Right (1, 100)
```

We expect `Right (1, 100)`: the original `helper 0 = 0 + 1 = 1`, and the
rewritten `helper 0 = 0 + 100 = 100`.
