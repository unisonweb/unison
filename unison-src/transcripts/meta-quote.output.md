# Quasiquote / splice syntax

`[| e |]` produces a `meta.Term meta.TermF` value representing the AST
of `e`. `${ x }` inside a quote splices a `meta.Term meta.TermF` value
into the quoted expression at that position.

(`[| / |]` rather than the design doc's `'{ / }` to avoid colliding
with Unison's existing thunk-with-effects type syntax `'{Ability} A`.)

``` ucm :hide
scratch/main> builtins.mergeio
```

A quoted Nat literal.

``` unison
fortyTwo : meta.Term meta.TermF
fortyTwo = [| 42 |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + fortyTwo : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view fortyTwo

  fortyTwo : meta.Term TermF
  fortyTwo = [| 42 |]
```

Splice a previously-built meta term — `[| ${fortyTwo} |]` is just
`fortyTwo`.

``` unison
fortyTwoAgain : meta.Term meta.TermF
fortyTwoAgain = [| ${fortyTwo} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + fortyTwoAgain : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

Round-trip: store the quote, eval it back to the original value.

``` unison
runQuoted : '{IO} Either Text Nat
runQuoted _ = match Meta.store fortyTwo with
  Left e -> Left e
  Right link -> Right (Meta.eval link)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runQuoted : '{IO} Either Text Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run runQuoted

  Right 42
```

Quoted references — `[| (Nat.+) |]` desugars to a `TermF.Ref` to the
builtin `Nat.+`. Parentheses around `Nat.+` keep it from absorbing the
trailing `|` token.

``` unison
plusRef : meta.Term meta.TermF
plusRef = [| (Nat.+) |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + plusRef : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view plusRef

  plusRef : meta.Term TermF
  plusRef = [| (Nat.+) |]
```

HOAS lambda — the binder `x` is introduced at quote-elaboration time
and becomes a `meta.ABT.Abs` wrapping a `meta.ABT.Var "x"` reference
inside the body.

``` unison
idQuoted : meta.Term meta.TermF
idQuoted = [| x -> x |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + idQuoted : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view idQuoted

  idQuoted : meta.Term TermF
  idQuoted =
    use meta.Term Term
    [| x -> x |]
```

Storing and evaluating the quoted identity function should give us a
runnable `Nat -> Nat` that returns its argument unchanged.

``` unison
runId : '{IO} Either Text Nat
runId _ = match Meta.store idQuoted with
  Left e -> Left e
  Right link ->
    f : Nat -> Nat
    f = Meta.eval link
    Right (f 7)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runId : '{IO} Either Text Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run runId

  Right 7
```
