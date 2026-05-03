# ADR-008: Specificity ordering — subsumption + lexical-inner-wins

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-005, ADR-009, ADR-010; `docs/implicits-plan.md` §1.3 ("Specificity ordering")

## Context

When more than one given matches a constraint goal, the resolver needs
a deterministic rule to either pick a winner or report ambiguity. Two
things happen frequently: (a) a generic given (`Show a => Show (List
a)`) and a specific given (`Show (List Nat)`) both match — most users
expect the specific one to win; (b) a local `given` declared in a `let`
block matches alongside a top-level given — most users expect the local
one to win.

The choice of rule is observable in user code and is hard to change
later: any change to the specificity rule reclassifies which programs
typecheck and which terms hash to what. Conservative is right.

## Options considered

### Option A: Subsumption + lexical-inner-wins

Two rules, applied in this order:
1. **Subsumption.** Given `A` is strictly more specific than given `B`
   iff there exists a substitution σ such that σ(B's *declared*
   conclusion) = A's *declared* conclusion, where σ may substitute B's
   quantified type variables only (A's quantified variables are treated
   as rigid). σ ≠ identity. The comparison uses *alpha-renamed declared*
   conclusions — not the post-unification specialised conclusions, since
   those equal the goal by construction. **One-way matching, not full
   unification.** If a unique most-specific candidate exists by
   subsumption, pick it.
2. **Lexical proximity.** Among candidates not separable by
   subsumption, a `given` introduced by a closer (more-inner) `let`
   binder wins over one introduced further out, regardless of type
   shape.
3. **Otherwise tie.** Report ambiguity, listing both candidates with
   namespace path and hash prefix.

> The "one-way matching, declared conclusions" detail was added after
> the Phase 1 spike: the first implementation compared post-unification
> conclusions and full-unified instead of one-way matched, producing
> incorrect specificity orderings. The spike's `oneWayMatch` is the
> reference implementation.

Pros: matches what users expect from "specific beats generic"; lets
local `given` shadow ambient givens as ADR-010 requires; deterministic
and simple to implement; aligns with the resolution algorithm in
`docs/implicits-plan.md` §1.3. Cons: subsumption-only ordering admits
cases where two candidates are unrelated under subsumption but share
common ground a human would consider "obviously equal" — these
ambiguate, which can frustrate users until they add a local given or
`@`-override.

### Option B: Priority annotations (Scala 3 style)

Allow givens to declare a priority level. A high-priority given beats
a default given. Pros: explicit; user controls the ordering. Cons:
introduces a new attribute on `given` declarations; prioritization
reasoning is non-local (you have to know all candidates' priorities);
encourages priority-tweaking battles when libraries disagree;
incompatible with Unison's "namespace = instance set, no global
arbiter" model from ADR-005.

### Option C: GHC overlap pragmas (`OVERLAPPING`, `OVERLAPPABLE`,
`INCOHERENT`)

GHC-style annotations on instances controlling which overlaps the
typechecker tolerates. **Rejected.** GHC's own users describe these
pragmas as footguns; the documentation warns against them; they
interact badly with cross-package coherence; they would import the
worst part of Haskell's instance system into a language explicitly
designed (ADR-005) to avoid that machinery.

## Decision

We adopt **Option A: subsumption + lexical-inner-wins, with
otherwise-ties reporting ambiguity.** No priority annotations, no
overlap pragmas.

## Consequences

- A specific given (`Show (List Nat)`) reliably wins over a generic
  one (`Show a => Show (List a)`) when both match. This is the
  textbook expected behavior and keeps stdlib design (Phase 5)
  uncomplicated.
- A local `given local : Ord a = …` reliably overrides any ambient
  `Ord a` given for the rest of its scope. ADR-010's local-given
  feature relies on this rule.
- Two genuinely-unrelated candidates (neither subsumes the other,
  same lexical depth) ambiguate. Users disambiguate via `@`-override
  (ADR-007) or a local `given`. This is intentional — we surface
  ambiguity rather than picking arbitrarily.
- Locks the language out of priority annotations. Future work that
  needs them (e.g. "default" instances) must either replace this ADR
  or use a different mechanism.
- Implementation: the spike (Phase 1) tests this on `Show (List Nat)`
  vs `Show (List a)` as a success criterion. The full elaborator
  (Phase 2.D) implements `mostSpecific` per the algorithm in
  §1.3 of the plan.
- Subsumption testing reuses unification machinery already in
  `Unison.Typechecker.Context`; no new theory beyond what the
  typechecker already has.
- Error rendering (Phase 4) for the "tied" case shows both candidates
  with enough information for the user to disambiguate; this is one
  of the 20+ golden error scenarios.
