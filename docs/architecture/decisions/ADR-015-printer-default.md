# ADR-015: Pretty-printer default behavior

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-007, ADR-014, ADR-016, ADR-017; `docs/implicits-plan.md` §7.1

## Context

After elaboration, an implicit-bearing call site has explicit `App`
nodes for each filled dictionary. The pretty-printer has to decide what
to render: the resolved arguments (verbose, but transparent) or the
source-style call without them (terse, but the resolution is invisible).

Whichever default we pick has to satisfy the round-trip property: for any
well-typed term `t`,
`parse(print(elaborate(t))) == elaborate(t)` after re-elaborating in the
namespace of origin. If the printer drops information the parser cannot
recover, content addressing breaks at the source layer.

## Options considered

### Option A: Elide implicit arguments by default; verbose mode shows them

Default `view`/`edit`/`find` output renders `print x`, not
`print @Show.nat x`. The signature still shows `=>` so the reader knows
implicits are involved. A verbose mode (CLI flag, UCM toggle, or LSP
preference) prints `@<dict>` for each resolved implicit. Pros: matches
how users wrote the code; matches the design intent ("givens are sugar
for ordinary parameters"); aligns with ADR-007's claim that `@`-overrides
are rare. Cons: source no longer carries the resolution, so
re-elaboration must reproduce it. If the namespace's given-set changes
between print and re-parse, re-elaboration may produce a different hash.

### Option B: Always show resolved implicit arguments

Every printed call site renders `@<dict>` arguments. Pros: hash-stable
round-trip — the printed source contains exactly the resolution that was
chosen, so re-parsing and elaborating recovers the same term regardless
of namespace state. Cons: visually heavy; user-written code rarely looks
this way; surfaces internal resolution choices that users intentionally
hid; defeats the ergonomics motivating implicits.

### Option C: Always elide

Never print resolved implicits; no verbose mode. Pros: maximally terse.
Cons: removes a debugging affordance; users have no way to ask "what did
the elaborator pick here?" without invoking a separate command.

## Decision

We recommend **Option A: elide implicit arguments by default, with a
verbose mode that shows `@<dict>` arguments,** pending ratification. The
default matches user expectation; the verbose mode preserves the
debugging affordance and gives a hash-stable form when needed.

The round-trip property must hold in both modes. In verbose mode the
property is trivial. In elide mode the printed source must re-elaborate
to the same term in the same namespace — this is a constraint on the
elaborator's determinism, not on the printer.

## Consequences

- **Unblocks** Phase 4 (pretty-printer and errors).
- **Depends on** ADR-014 (so elaborator output is canonical and
  re-elaboration is deterministic) and ADR-016 (which defines what
  happens when re-elaboration changes hash).
- **Constrains** Phase 2.D (elaborator) — given a fixed namespace and
  fixed input, resolution must produce the same chosen dictionaries
  every time. Already required by hashing; this ADR makes the printer
  rely on it.
- Verbose mode becomes part of the UCM surface (ADR-017). Naming and
  flag spelling are decided there.
- Required tests: round-trip property test (elide and verbose modes) on
  a corpus including local givens, chained givens, HKT givens, and
  explicit `@`-overrides; golden printer output for the canonical
  examples in `docs/implicits-plan.md` §1.2; LSP hover renders verbose
  form on synthesized arguments per Phase 2.F.
- An elide-mode print that is then *re-elaborated in a different
  namespace with different givens* may produce a different hash. This
  is the user-facing meaning of "namespace = instance set"; ADR-016
  governs the failure mode if the change becomes ambiguous.
