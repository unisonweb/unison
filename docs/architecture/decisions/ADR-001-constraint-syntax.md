# ADR-001: Constraint syntax — Haskell-style `C =>`

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-002, ADR-003, ADR-006, ADR-010; `docs/implicits-plan.md` §1.2

## Context

Unison is gaining Scala-3-style implicit parameters under the working name
`givens`. A function that *consumes* a given needs syntax in its type
signature so that the elaborator can identify which leading parameters are
implicit. This is a one-way door at the surface level — once we ship a
syntax, every library written with implicits will use it forever and any
change carries an ecosystem-wide migration cost.

The choice here is independent of the underlying semantics (the constraint
desugars to an ordinary leading argument either way) but it strongly shapes
the look-and-feel of the feature, the ergonomics of multiple constraints,
and what "implicit-bearing function" looks like on the page.

## Options considered

### Option A: Haskell-style `C =>` in the signature

A constraint precedes the rest of the type: `Show a => a -> Text`.
Multiple constraints group with parentheses: `(Monad m, Traversable t) =>
…`. The `=>` arrow is distinct from the value-level `->`, so a reader can
tell at a glance which arguments will be filled implicitly. Pros: familiar
to Haskell, PureScript, Idris, Rust trait-bound users; visually compact;
groups well; aligns with how subclassing-via-records reads in `docs/implicits-plan.md` §1.2. Cons: `=>` is one new symbolic keyword and its
ordering vs `==>` matters in the lexer.

### Option B: Scala 3 `using` parameters in the signature

Mark each implicit parameter individually, e.g. `(using Show a) -> a ->
Text`. Pros: parameters are listed in left-to-right order, matching the
elaborated form one-for-one; uniform with the local `using`-bound form.
Cons: verbose for the common case of multiple constraints; visually
heavier; loses the at-a-glance "this function takes a typeclass" cue;
requires `using` to be a hard keyword in type-syntax position.

### Option C: No type-level constraint syntax (only `using`-bound locals)

Don't introduce any signature-level constraint syntax at all; users would
construct dictionaries explicitly and bind them with `using` at call
sites. Pros: minimal language addition. Cons: defeats the point — every
caller has to know about and manually thread every dictionary; eliminates
the "constraint" abstraction users came for; unusable in practice.

## Decision

We adopt **Option A: Haskell-style `C =>` in signatures.** The constraint
arrow `=>` precedes the rest of the type and groups with parentheses for
multiple constraints. It desugars to a leading positional parameter
tagged as implicit (per the elaborator).

## Consequences

- The `=>` token becomes a hard symbolic keyword in the type grammar.
  `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` (`symbolyKw`) must
  list `=>` *after* `==>` so the longer match wins.
- Users get a familiar surface reminiscent of Haskell/PureScript/Idris.
  Existing tutorials from those ecosystems carry over with minor edits.
- Multiple constraints fall out naturally as tuple-style grouping
  (`(C1 a, C2 b) =>`); no special multi-constraint syntax is needed.
- Locks us out of repurposing `=>` for anything else in type position
  (e.g. higher-kinded arrows, effect arrows). Effect arrows already use
  the orthogonal `->{e}` form, so the conflict surface is small.
- ADR-019 must specify how the implicit-ness is represented in `Type.F`;
  the surface choice here doesn't decide that, but it commits us to
  preserving the distinction at parse time.
- Pretty-printer default (ADR-015) renders `=>` rather than `using`
  parameters, matching how users will write signatures.
