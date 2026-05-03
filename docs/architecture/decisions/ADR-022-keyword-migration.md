# ADR-022: Keyword migration plan for `given` and `summon`

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-001, ADR-006, ADR-018; `docs/implicits-plan.md` §10 (risk: "Keyword migration breaks existing code")

## Context

The implicits feature introduces two new keywords: `given` (declaration
prefix and `let`-block form) and `summon` (explicit summon expression).
Today neither is reserved: the `keywords` set in
`unison-syntax/src/Unison/Syntax/ReservedWords.hs` does not contain
either, so any user code can have a definition named `given` or `summon`,
or use them as local variable names. Existing user code may currently
do exactly that.

The `=>` token is similar in shape but easier: it is purely symbolic
and already disambiguated only against `==>` (currently in
`reservedOperators`). `=>` does not collide with any user-writable
identifier — only with potential operator definitions, of which `==>` is
the existing precedent.

This ADR addresses `given` and `summon` only; `=>` is governed by
ADR-001's lexer ordering note.

## Options considered

### Option A: Hard break with a one-release deprecation warning

The next release after the implicits feature stabilizes adds `given` and
`summon` to the `keywords` set. The release *before* that ships a
deprecation warning: any definition or local variable named `given` or
`summon` parses successfully but emits a warning pointing to a migration
note. Pros: user code gets one full release cycle to rename; the
keyword space ends up clean; aligns with how mainstream languages
handle reserved-word additions; per ADR-018, the feature flag delays
the hard break until library authors have opted in. Cons: any user with
a definition named `given` or `summon` must rename it within one
release cycle; tooling must recognize and migrate (search-and-replace
is straightforward).

### Option B: Prefix sigils (`#given`, `#summon`)

Use `#given` as the declaration prefix and `#summon` as the expression
form. Pros: no keyword reservation needed; bare `given`/`summon` remain
legal identifiers indefinitely. Cons: visually uglier; inconsistent
with how `let`, `if`, `match`, `cases`, `handle` etc. read; awkward in
documentation and tutorials; loses the "fits like a glove" goal.

### Option C: Non-reserved with parser-level disambiguation

Keep `given` and `summon` non-reserved in the lexer; rely on parser
context to recognize them as keywords only where they syntactically
make sense (top-level declaration position, expression-application
position). Pros: zero migration cost; existing identifiers keep
working. Cons: context-sensitive lexing/parsing is fragile;
disambiguation rules become subtle (`let given = …` could be a local
given declaration *or* a let-bound identifier called `given`); error
messages get harder; precedent in other languages (Scala 3 itself)
shows this is a long tail of edge cases.

## Decision

We recommend **Option A: hard break with a one-release deprecation
warning,** pending ratification. Concretely:

1. **Release N (implicits behind a flag, default off, per ADR-018).**
   Lexer recognizes `given` and `summon` as reserved-when-flag-on.
   When the flag is off, `given` and `summon` parse as identifiers as
   today, but with a deprecation warning. The warning links to the
   migration note.
2. **Release N+1 (flag flips to on by default, per ADR-018).**
   `given` and `summon` join the `keywords` set in
   `unison-syntax/src/Unison/Syntax/ReservedWords.hs` unconditionally.
   Code that still uses them as identifiers fails to parse with a
   structured error directing the user to rename.

The deprecation window is one release. The migration note (in
`docs/implicits-plan.md` and the migration guide) provides a
search-and-replace recipe.

## Consequences

- **Unblocks** Phase 6 (release) — the keyword reservation has a
  defined timeline.
- **Depends on** ADR-018 (the per-project flag is what allows the
  deprecation warning to fire only against projects that *haven't* opted
  in yet, while opted-in projects already use the keywords as
  keywords).
- **Forces** an edit to
  `unison-syntax/src/Unison/Syntax/ReservedWords.hs`: the `keywords` set
  grows by `"given"` and `"summon"`. (No change for `=>`/`==>`: per
  ADR-001 the lexer ordering already handles symbolic precedence.)
- Required tests: a deprecation-warning fixture for Release N (a file
  with `given = …` or `let summon = …` produces a warning); a
  hard-break fixture for Release N+1 (same file fails with a clear
  error); a migration-script test if we ship one; a regression test
  asserting `=>` still parses correctly when adjacent to `==>`.
- Locks `given` and `summon` as keywords forever; freeing them later
  would itself be a breaking change.
- Documentation obligation: the migration guide must include the
  search-and-replace recipe and a list of common identifier names that
  conflict (e.g. functions called `given` in test fixtures).

## Open question

Whether to also reserve `using` proactively, even though the design
chose `=>` over Scala-3 `using` (per ADR-001). If a future ADR
introduces explicit `using`-bound parameter syntax, we would want the
keyword available. Recommendation: defer; introduce only when needed,
following the same one-release deprecation pattern.
