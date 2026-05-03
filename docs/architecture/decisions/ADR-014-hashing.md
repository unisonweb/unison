# ADR-014: Hashing of givens and elaborated terms

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-002, ADR-004, ADR-013, ADR-019, ADR-020; `docs/implicits-plan.md` §1.1, §5.6

## Context

Implicit parameters interact with hashing in three places:

1. **Given declarations.** A `given Show.nat : Show Nat = …` is just a
   term whose namespace entry happens to carry a given-tag. Does the
   declaration hash differently from an ordinary term?
2. **Elaborated terms.** A caller `print x` becomes
   `print @ Show.nat x` after elaboration. What does the term hash
   against?
3. **Branch (causal) hash.** Marking a definition `given` changes
   namespace metadata. Does the branch hash reflect that?

Each of these has a tempting "introduce a new hashing rule" answer. The
goal of this ADR is the opposite: confirm that the existing
infrastructure already does the right thing, so we add no new hashing
rule.

## Options considered

### Option A: No new hashing rule — rely on existing infrastructure

- Given declarations hash like ordinary terms. The body has a type
  (per ADR-019, including any leading implicit-arrow parameters) and the
  ABT `App`/`Lam`/`Ref` shape from `unison-core/src/Unison/Term.hs`. The
  given-tag is *not* part of the term's hash — it lives in namespace
  metadata per ADR-002/013.
- Elaborated terms hash against resolved dictionary hashes. Once the
  elaborator has filled in implicit arguments as plain `App` nodes
  pointing at the chosen `Referent`s, the term reduces to ordinary
  function application; the existing term-hash machinery already hashes
  it correctly.
- Branch hash already includes `MdValues` in its token list, see
  `unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs:34`. Storing the
  given-tag inside `MdValues` (per ADR-013) means marking and unmarking
  automatically changes the causal hash — the same mechanism that tracks
  any other metadata edit.

Pros: zero churn; consistent with ADR-004 ("runtime/codegen untouched");
no new "given hashing" surface area in `unison-hashing-v2`. Cons: relies
on ADR-019 settling cleanly so that a type containing `=>` hashes as a
distinct type from one containing only `->`, and relies on ADR-013
landing the tag inside `MdValues` rather than a new field.

### Option B: Introduce a separate "given declaration" hashing rule

Treat `given` declarations as a distinct hashable kind so the keyword is
visible in the hash. Pros: hash carries the "this was declared given"
signal even if metadata is lost. Cons: contradicts ADR-002 (givenness is
namespace metadata, not term-level); makes alias/move/unmark operations
semantically heavy; rules out demoting an upstream given without
rehashing.

### Option C: Hash elaborated terms against pre-elaboration form

Strip implicit arguments before hashing the elaborated term so two
namespaces with different givens but the same source produce the same
hash. Pros: source-text identity. Cons: destroys content-addressing
guarantees; old code's resolution would be hash-equivalent to new code
with a different resolution; breaks the core "resolution is baked into
the hash" principle (ADR-002, principle 2).

## Decision

We recommend **Option A: no new hashing rule,** pending ratification.
Given declarations hash like ordinary terms; elaborated terms hash
against resolved dictionary hashes via the existing `App` machinery; the
branch hash captures givenness through the existing `MdValues`-in-tokens
path. The change to `Type.F` decided in ADR-019 changes the type-hash
namespace once but produces only new types going forward.

## Consequences

- **Unblocks** Phase 2.E (hashing and update semantics) — no new
  `unison-hashing-v2` schema work.
- **Depends on** ADR-013 (storage shape is `MdValues`-based so the
  branch-hash path already covers tagging) and ADR-019 (so the
  type-grammar change is the only new hash-affecting addition).
- **Reinforces** ADR-016: an `update` that re-resolves to a different
  given produces a different term hash, leaving old code's hash intact —
  the foundation of the "ambiguity-free re-resolution silently produces
  a new hash" rule.
- Locks us into elaborator output that is *already* a plain `App`
  (i.e. the elaborator does not introduce any new ABT constructor).
- Required tests: golden-hash test fixing the hash of a representative
  elaborated term; mark/unmark of a given changes the branch hash but
  not the term hash; aliasing a given preserves the term hash; the
  type-hash break for `=>` types is documented and golden-tested.
- Should ADR-019 instead choose a side-table representation (its
  Option B), this ADR's claim that elaborated terms hash via existing
  `App` machinery still holds, but the "new types only" framing weakens;
  ADR-019 must record any cross-impact.
