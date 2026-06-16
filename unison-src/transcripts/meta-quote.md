# Quasiquote / splice syntax

`[| e |]` produces a `meta.Term meta.TermF` value representing the AST
of `e`. `${ x }` inside a quote splices a `meta.Term meta.TermF` value
into the quoted expression at that position.

(`[| / |]` rather than the design doc's `'{ / }` to avoid colliding
with Unison's existing thunk-with-effects type syntax `'{Ability} A`.)

```ucm :hide
scratch/main> builtins.mergeio
```

A quoted Nat literal.

```unison
fortyTwo : meta.Term meta.TermF
fortyTwo = [| 42 |]
```

```ucm
scratch/main> add
scratch/main> view fortyTwo
```

Splice a previously-built meta term — `[| ${fortyTwo} |]` is just
`fortyTwo`.

```unison
fortyTwoAgain : meta.Term meta.TermF
fortyTwoAgain = [| ${fortyTwo} |]
```

```ucm
scratch/main> add
```

Round-trip: store the quote, eval it back to the original value.

```unison
runQuoted : '{IO} Either Text Nat
runQuoted _ = match Meta.store fortyTwo with
  Left e -> Left e
  Right link -> Right (Meta.eval link)
```

```ucm
scratch/main> run runQuoted
```

Quoted references — `[| (Nat.+) |]` desugars to a `TermF.Ref` to the
builtin `Nat.+`. Parentheses around `Nat.+` keep it from absorbing the
trailing `|` token.

```unison
plusRef : meta.Term meta.TermF
plusRef = [| (Nat.+) |]
```

```ucm
scratch/main> add
scratch/main> view plusRef
```

HOAS lambda — the binder `x` is introduced at quote-elaboration time
and becomes a `meta.ABT.Abs` wrapping a `meta.ABT.Var "x"` reference
inside the body.

```unison
idQuoted : meta.Term meta.TermF
idQuoted = [| x -> x |]
```

```ucm
scratch/main> add
scratch/main> view idQuoted
```

Storing and evaluating the quoted identity function should give us a
runnable `Nat -> Nat` that returns its argument unchanged.

```unison
runId : '{IO} Either Text Nat
runId _ = match Meta.store idQuoted with
  Left e -> Left e
  Right link ->
    f : Nat -> Nat
    f = Meta.eval link
    Right (f 7)
```

```ucm
scratch/main> run runId
```
