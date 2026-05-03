# ADR-016 scenario (c): a new ambiguous given doesn't disturb old code

The exit criterion from `docs/implicits-plan.md` §5.6: *"introducing
a second matching given causes new code to fail with ambiguity but
doesn't disturb existing code."*

ADR-014 explains why old code is undisturbed: an elaborated term
hashes against the resolved dictionary's hash via the existing `App`
machinery, so the old term has a fixed reference and a fixed hash
regardless of what new givens appear. New code re-elaborates fresh
and the resolver (D4) reports `Ambiguous` per ADR-005.

For D3 we exercise the parser/typechecker plumbing only: the
resolver pool is supplied externally (the namespace ambient pool is
empty until D4), so the "Ambiguous" error itself surfaces in chunk D4.
Here we pin the codebase shape — two equally-good `Show Nat` givens
present — and confirm pre-existing code's hash is unchanged.

``` ucm :hide
> builtins.merge
```

Define `Show`, the original given `Show.nat`, and a consumer `foo`
that depends on it.

``` unison
unique type Show a = Show (a -> Text)

given Show.nat : Show Nat = Show.Show Nat.toText

foo : Nat -> Text
foo n = match Show.nat with Show.Show f -> f n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Show a

  + foo      : Nat -> Text
  + Show.nat : Show Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Capture the original hash of `foo` (call it H1).

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #9qoq16jvg3   Term   foo
```

Now add a *second* given of the same type, `Show.alternateNat`. The
elaborator (D4) will see two equally-applicable candidates for any
fresh `Show Nat` constraint and must report `Ambiguous`.

``` unison
given Show.alternateNat : Show Nat = Show.Show (n -> "n=" ++ Nat.toText n)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Show.alternateNat : Show Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

`foo`'s hash is unchanged (still H1). It was elaborated against
`Show.nat` at original definition time and that resolution is baked
into the term's hash forever (ADR-014, ADR-002 principle 2).

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #9qoq16jvg3   Term   foo

> names Show.nat

  'Show.nat':
  Hash          Kind   Names
  #06rjmokfst   Term   Show.nat

> names Show.alternateNat

  'Show.alternateNat':
  Hash          Kind   Names
  #igvikkkjns   Term   Show.alternateNat
```
