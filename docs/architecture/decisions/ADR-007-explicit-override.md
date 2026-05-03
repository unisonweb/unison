# ADR-007: `@`-positional explicit override at call sites

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-001, ADR-006, ADR-010; `docs/implicits-plan.md` §1.2 ("Explicit summon and override"), §12 (lexer/parser disambiguation)

## Context

Even with implicit resolution working, users need a way to pass a
specific dictionary at a call site. The two main use cases are
(a) testing — passing a mock `Show` to confirm a function calls
`Show.show` with the expected formatting; and (b) breaking ambiguity
locally — when two givens both match and the user wants this *one*
call to use a particular one without introducing a `let`-bound local
given.

The form must read clearly, must compose with chains of implicit
parameters (`f @ d1 @ d2 x`), and must not clash with the rest of the
expression grammar — `@` is already used in patterns
(`x @ Foo a`, `parser-typechecker/src/Unison/Syntax/TermParser.hs`
line ~410) and as a doc-comment prefix
(`parser-typechecker/src/Unison/Syntax/Parser/Doc.hs` line ~163).

## Options considered

### Option A: `@`-positional override (`f @ d x`)

`f @ d x` parses as "apply `f` to `d` in its next implicit slot, then
apply to `x`." Multiple `@`-overrides chain left-to-right:
`f @ d1 @ d2 x` fills the first two implicit slots. Pros: terse, fits
on one line; doesn't need a name for the implicit parameter; the
`@`-token is already in the symbolic-keyword table; one-character
visual cue that "this is non-standard arg passing." Cons: parser
disambiguation needed against as-patterns and doc syntax — addressed
by context (expression position vs pattern position vs doc position).

### Option B: Named-argument syntax (`f (using = d) x`)

Each implicit parameter gets a name; the override picks the name.
Pros: unambiguous when a function has multiple implicits of unrelated
types; survives reordering of implicits in the signature. Cons:
implicit parameters typically have no good names — they're identified
by their type, not by a label; introduces a new "named argument"
machinery that doesn't exist elsewhere in Unison; verbose for the
common case of one implicit; redundant with the type-based positional
order that ADR-001 already establishes.

### Option C: Apply by type (`f [Show Nat = d] x`)

Bracket form keying on the type rather than the position. Pros: works
under reordering of implicits. Cons: even more verbose; the type
already determines which slot is filled, so the bracket adds no
information at a typical call; brings new syntax for a corner case.

## Decision

We adopt **Option A: `@`-positional explicit override at call sites.**
`f @ d` fills the next implicit slot of `f` with `d`; chained
`@`-applications fill subsequent slots in order.

## Consequences

- Phase 2.A's parser must disambiguate three uses of `@` by context:
  (1) as-patterns inside `cases`/`match` (existing); (2) doc-prefix
  inside `Doc` blocks (existing); (3) explicit-override in expression
  context (new). The lexer already produces `@` as a symboly keyword
  (`unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` line ~572); the
  three meanings live in disjoint parser states.
- `@`-overrides are positional, so changing the order of implicit
  parameters in a function's signature is a breaking change for any
  caller that uses `@`-override. This is acceptable because reordering
  implicits in a signature is itself a breaking change (it's a
  type-shape change), and `@` users will see the same error any other
  caller would.
- Testing pattern is straightforward: `myFunction @ mockShow input`.
  No need to introduce a local `given` or unbind the ambient one.
- Pretty-printer verbose mode renders implicit applications as
  `f @ d1 @ d2 args`, the same form a user would write — verbose
  output is parseable as source.
- Forecloses adding "named implicit arguments" later without grammar
  conflict. If demand surfaces, a `(name = value)` extension to
  application-syntax could coexist with `@`-positional, but is not
  planned.
- Editor tooling (Phase 2.F LSP): hover on a synthesized `@d`
  argument shows the resolved given's name and hash. Source-position
  hover skips synthesized args because they don't exist in source.
