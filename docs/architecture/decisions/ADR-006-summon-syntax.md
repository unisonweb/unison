# ADR-006: `summon T` is the explicit summon form

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-001, ADR-007, ADR-022; `docs/implicits-plan.md` §1.2 ("Explicit summon and override"), §12 (lexer)

## Context

Most uses of givens never need an explicit "fetch the dictionary" form:
calls insert constraint arguments automatically and the user writes
`Show.show x` rather than `Show.show (the (Show a)) x`. Still, an
explicit summon form is needed in three situations: (a) when the
dictionary itself is the value being passed (`given local : Ord a =
Ord.flip (summon (Ord a))`), (b) when writing a local given that wraps
or transforms an ambient one, and (c) when teaching the resolver
behavior in tests and docs.

The choice is therefore about syntax for a relatively rare construct,
but one that shows up in every nontrivial example in the design plan.
The trigger for this ADR was discovering that the otherwise-attractive
`?T` form clashes with Unison's existing `Char` literal syntax.

## Options considered

### Option A: `summon T`

A keyword `summon` followed by the type. Pros: reads as English ("summon
a `Show Nat`"); zero conflict with existing surface syntax (`summon` is
not currently reserved per `unison-syntax/src/Unison/Syntax/ReservedWords.hs`);
unambiguous in any expression context. Cons: `summon` is currently a
legal identifier, so introducing it as a keyword breaks any program
that names a binding `summon` (ADR-022 covers the migration).

### Option B: `?T` prefix sigil

Scala 3 / Idris-flavoured: `?T` reads "an instance of `T`, please."
Compact, sigil-based, no keyword needed. **Rejected** because it
clashes with Unison's `Char` literal syntax. The lexer at
`unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` line 489 reads
`character = Character <$> (char '?' *> (spEsc <|> LP.charLiteral))`
— `?` is the `Char` literal prefix. `?T` in expression context would
be ambiguous with a malformed character literal, and tightening the
disambiguation to handle "`?` followed by a type-shaped expression"
would either break existing programs or require deep parser changes.

### Option C: `the T` (Idris)

Idris uses `the : (a : Type) -> a -> a` for type-ascription / explicit
elaboration. Pros: established in another implicit-aware language;
short. Cons: "the" is an extremely common English word and a frequent
identifier choice (`the user`, `the request`); migration is painful;
reads less actively than `summon`; the Idris meaning is closer to type
ascription than implicit summoning, so importing the keyword imports
confusion.

### Option D: `given T` (Scala 3 expression form)

Scala 3 uses `given` in expression position too: `summon[T]` with
`given` declarations. Pros: keyword reuse with the declaration form.
Cons: `given` already means "declare a given" in our top-level
syntax; reusing it for "fetch a given" creates two unrelated meanings
parsed by context; ambiguous when an in-let `given` declaration is
parsed (Phase 2.A's parser would have to disambiguate
`given X : T = …` from `given T` as an expression).

## Decision

We adopt **Option A: `summon T` as the explicit summon form.** It
reads naturally, has no existing-syntax conflict, and is unambiguous
in every expression context. The `?T` form is rejected on the basis
of the `Char`-literal lexer clash discovered during design.

## Consequences

- `summon` becomes a reserved keyword. ADR-022 covers the migration
  for any user code that currently names a binding `summon`. The
  reserved-words list (`unison-syntax/src/Unison/Syntax/ReservedWords.hs`)
  must add `summon`.
- The form takes a type, not an expression: `summon (Show Nat)`,
  `summon (Ord a)`. The parser knows it's in type context after
  `summon`.
- In every example in the design plan that needs an explicit
  dictionary handle, `summon` reads naturally; reviewers report the
  surface looks closer to English than to a sigil soup.
- Locks us into "explicit summon is verbose by design." Users should
  rarely need it; if surveys find frequent use, that's a signal the
  *implicit* path has a hole, not a signal to add a sigil shortcut.
- Forecloses repurposing `summon` for any other language feature.
  Given how specific the word is, the loss is small.
- Pretty-printer (Phase 4 / ADR-015) renders explicit summons as
  `summon T`, including in verbose mode where `@d` overrides are
  elaborated.
