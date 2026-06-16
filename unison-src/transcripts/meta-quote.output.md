# Quasiquote / splice syntax

`[| e |]` produces a `meta.Term meta.TermF` value representing the AST
of `e`. `${ x }` inside a quote splices a `meta.Term meta.TermF` value
into the quoted expression at that position.

(`[| / |]` rather than the design doc's `'{ / }` to avoid colliding
with Unison's existing thunk-with-effects type syntax `'{Ability} A`.)

``` ucm :hide
scratch/main> builtins.mergeio
```

A quoted Nat literal — desugars to a `meta.Term meta.TermF` containing
`Lit (LitNat 42)`.

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

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
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

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

A round-trip check: store the quote, eval it back, confirm the result.

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
