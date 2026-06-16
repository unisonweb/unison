# Quasiquote: meta-Unison code, ergonomically

`[| e |]` quotes a Unison expression into a `meta.Term meta.TermF`
value — its abstract syntax tree, not its runtime value. `${ x }`
inside a quote splices a `meta.Term meta.TermF` back in at that
position. Together they let you build, inspect, transform, and run
Unison code as ordinary first-class data.

```ucm :hide
scratch/main> builtins.mergeio
```

## Quote, view, run

Quote a function with a HOAS-bound binder. `x` is introduced at
quote-elaboration time and becomes a `meta.ABT.Var` reference inside
the body; `Nat.+` and `1` lift to a `meta.TermF.Ref` and a
`meta.TermF.Lit` respectively.

```unison
add1 : meta.Term meta.TermF
add1 = [| x -> (Nat.+) 1 x |]
```

```ucm
scratch/main> add
scratch/main> view add1
```

`view` round-trips through the pretty printer — the desugared
constructor soup prints back as `[| ... |]` instead.

Now compile that quoted AST into a runnable function and apply it:

```unison
runAdd1 : '{IO} Either Text Nat
runAdd1 _ = match Meta.store add1 with
  Left e -> Left e
  Right link ->
    f : Nat -> Nat
    f = Meta.eval link
    Right (f 41)
```

```ucm
scratch/main> run runAdd1
```

## Composition via splice

`${ x }` inside a quote splices in an existing `meta.Term meta.TermF`
at that position. This makes meta-level combinators ordinary Unison
functions:

```unison
-- Build the AST of `x -> f (g x)`.
compose : meta.Term meta.TermF -> meta.Term meta.TermF -> meta.Term meta.TermF
compose f g = [| x -> ${f} (${g} x) |]
```

```ucm
scratch/main> add
```

Compose `add1` with itself — the result should compute `n -> n + 2`.

```unison
add2 : meta.Term meta.TermF
add2 = compose add1 add1
```

```ucm
scratch/main> add
scratch/main> view add2
```

Note how `view add2` shows the *spliced-and-quoted* form, with the
spliced occurrences of `add1` already substituted into the lambda
body.

```unison
runAdd2 : '{IO} Either Text Nat
runAdd2 _ = match Meta.store add2 with
  Left e -> Left e
  Right link ->
    f : Nat -> Nat
    f = Meta.eval link
    Right (f 40)
```

```ucm
scratch/main> run runAdd2
```

`Right 42`: `add2 40 = 1 + (1 + 40)`.

## Without quasiquote

For comparison, here's what `add1` looks like spelled out by hand —
this is essentially what the desugarer emits behind the scenes, and
each quoted AST node costs about five lines of wrapping. Quasiquote
is the same data, two characters of overhead per node.

```unison
add1Manual : meta.Term meta.TermF
add1Manual =
  empty = Set.Set Map.Tip
  wrap : meta.TermF (meta.Term meta.TermF) -> meta.Term meta.TermF
  wrap inner = meta.Term.Term empty (meta.ABT.Tm inner)
  wrapVar : Text -> meta.Term meta.TermF
  wrapVar name = meta.Term.Term empty (meta.ABT.Var (meta.Name.Name name))
  wrapAbs : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
  wrapAbs name body = meta.Term.Term empty (meta.ABT.Abs (meta.Name.Name name) body)
  ref : Text -> meta.Term meta.TermF
  ref n = wrap (meta.TermF.Ref (meta.Reference.ReferenceBuiltin n))
  lit : meta.Literal -> meta.Term meta.TermF
  lit l = wrap (meta.TermF.Lit l)
  app : meta.Term meta.TermF -> meta.Term meta.TermF -> meta.Term meta.TermF
  app f x = wrap (meta.TermF.App f x)
  -- x -> (Nat.+) 1 x
  wrap (meta.TermF.Lam (wrapAbs "x"
    (app (app (ref "Nat.+") (lit (meta.Literal.LitNat 1))) (wrapVar "x"))))
```

```ucm
scratch/main> add
```

Both definitions should typecheck and evaluate identically; verify
they do by storing each, evaluating, and comparing the runtime
results.

```unison
quotedVsManual : '{IO} Either Text Boolean
quotedVsManual _ = match Meta.store add1 with
  Left e -> Left e
  Right qLink -> match Meta.store add1Manual with
    Left e -> Left e
    Right mLink ->
      qf : Nat -> Nat
      qf = Meta.eval qLink
      mf : Nat -> Nat
      mf = Meta.eval mLink
      Right (qf 10 == mf 10 && qf 100 == mf 100)
```

```ucm
scratch/main> run quotedVsManual
```
