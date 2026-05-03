# ADR-005: Coherence — ambiguity at call site, no global uniqueness

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-002, ADR-008, ADR-021; `docs/implicits-plan.md` §1.1 principle 3, §1.3

## Context

A typeclass-style implicit system has to decide what happens when two
matching instances are visible. Haskell enforces *global coherence*:
across the entire program (all transitive imports), at most one instance
exists for each `(class, type)` pair. Violations are compile errors
(orphan instances, overlapping instances). This requires a global
registry, orphan-instance rules, and rejects programs that two libraries
could otherwise compose.

Unison's namespace model is incompatible with global registries. There
is no transitive-import notion: a file pulls in exactly the names its
namespace exposes, and two libraries' worth of names can sit alongside
without conflict until a user calls into both. The `MdValues` slot lives
per-namespace, not globally.

## Options considered

### Option A: Per-call-site ambiguity (no global uniqueness)

The "instance set" at a given program point is exactly the set of
visibly-tagged givens in scope. Resolution at a call site searches that
set. If exactly one given matches, use it. If multiple match (and
specificity per ADR-008 doesn't pick a winner), report ambiguity at the
call site naming both candidates with their namespace paths and hash
prefixes — the same UI TDNR already uses for ambiguous names. Pros:
fits Unison's namespace model; zero global state; two libraries that
each define `given Show.nat` compose freely until a third site sees
both; downstream users can disambiguate by shadowing or by `@`-override
(ADR-007); errors are local and actionable. Cons: a function might
typecheck in one namespace and fail in another with strictly more
visible givens; users must learn that "more imports can break me."

### Option B: Haskell-style global coherence with orphan rules

Maintain a global registry of `(class, type) -> instance` and reject
duplicates. Pros: a function that typechecks anywhere typechecks
everywhere; well-understood semantics; predictable. Cons: requires a
global registry that doesn't exist in Unison and is incompatible with
how namespaces, aliasing, and downstream reclassification (ADR-002)
work; orphan rules are notoriously painful in Haskell and would need
to be reinvented for Unison's name-based world; rules out the
"namespace = view onto givens" property; requires defining what "the
program" is in a UCM session that touches many namespaces.

## Decision

We adopt **Option A: coherence by call-site ambiguity. The namespace
*is* the instance set.** Resolution searches visible givens; multiple
matches that don't disambiguate via ADR-008's specificity rules are
reported at the call site with both candidates. There is no global
registry, no orphan rule, and no cross-namespace uniqueness check.

## Consequences

- Two libraries can each define `given Show.nat` and ship to Share
  without a conflict. They only conflict when a user pulls both into
  one namespace and writes a `Show Nat`-using term *in that
  namespace*.
- Ambiguity errors are reused from TDNR's existing machinery: same
  formatting, same near-miss reporting, same "did you mean…"
  affordances. Implementation effort is low.
- A function written against a small namespace may fail to elaborate
  when that namespace is enlarged. This is a real ergonomic cost.
  Users learn to disambiguate via local `given` shadowing (ADR-010)
  or `@`-override (ADR-007) at the call site.
- ADR-008's specificity rules (subsumption + lexical-inner-wins) are
  the only knob for resolving multi-candidate cases. Rejected:
  GHC-style `OVERLAPPING`/`OVERLAPPABLE` pragmas — they encourage
  fragile ordering tricks.
- ADR-021 (merge semantics) inherits a defined story: when two
  branches both add givens for the same type at the same name, the
  merge tooling has to surface that and let the user pick.
- Locks us into "which set of givens is in scope" being a
  namespace-local question. Anything that wants global behavior
  (e.g., a "canonical `Show Nat`" library lemma) has to be solved by
  social convention or by Phase 5's stdlib seed, not by the
  resolver.
- A future reviewer who finds themselves wanting "global coherence"
  should re-read this ADR and ADR-002 first; the answer is "by
  design, no."
