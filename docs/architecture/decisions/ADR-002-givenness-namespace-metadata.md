# ADR-002: Givenness is namespace metadata, not a term-level tag

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-004, ADR-005, ADR-013, ADR-014, ADR-020; `docs/implicits-plan.md` §1.1 principle 4, §5.2

## Context

A "given" is an ordinary Unison term that the elaborator is allowed to
pick up implicitly. Something has to record the bit "this term is
eligible for implicit resolution." Where that bit lives — in the term, in
the type, or in the namespace — has consequences for hashing, aliasing,
forking, and how downstream consumers can disagree with upstream about
what counts as a given.

Unison's identity story is built around content-addressed term hashes:
two definitions with the same hash are the same term forever, history
pointers don't break, and aliases are free. Anything that "tags" a term
must not perturb that hash, or the abstraction breaks down. At the same
time the namespace already has a metadata slot — `MdValues` per
`(NameSegment, Referent)` in
`codebase2/codebase/U/Codebase/Branch/Type.hs` — designed for exactly
this kind of attribution.

## Options considered

### Option A: Namespace metadata tag

Mark the (name, hash) binding in the current namespace as "given." The
term hash is unchanged; the *branch* (causal) hash changes, since branch
hashing in `unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs` includes
`MdValues` in its tokens. Pros: aliases are independent (a downstream
namespace can demote an upstream given or promote a non-given without
touching its hash); fits an existing schema slot; matches Unison's
"namespace = view onto the codebase" model. Cons: requires plumbing
metadata through name lookup and all read sites (presently `MdValues` is
written but barely read); requires ADR-020 to settle storage shape.

### Option B: Term-level tag (special term form)

Wrap given terms in a new ABT node like `Given e` or introduce a
distinct top-level declaration form. Pros: fully self-describing —
fetching the term tells you it's a given. Cons: changes the term hash
(every given hashes differently from a same-bodied non-given), so
flipping givenness becomes a destructive update; `unison-runtime` and
`unison-hashing-v2` both grow a concept; aliases can't disagree; rules
out the downstream-reclassification story.

### Option C: Type-level marker

Encode givenness in the type, e.g. with a phantom newtype `Given a`.
Pros: zero runtime/storage changes. Cons: can't tag an existing
upstream definition without changing its type (and therefore its hash);
forces every given to be defined as a `Given`-wrapped value; cuts off
the "promote/demote without touching the term" use case.

## Decision

We adopt **Option A: givenness is recorded as namespace metadata** on
the (name, hash) binding, stored in or alongside `MdValues`. Term hashes
are untouched by tagging; branch hashes change (already true for any
metadata edit); the runtime sees nothing.

## Consequences

- Term hashes are stable across `mark.given` / `unmark.given`. Aliases,
  history pointers, and dependent terms keep working unchanged.
- Branch hashes legitimately change when givenness changes — this is
  the same behavior any `MdValues` edit produces today, and is what
  lets downstream namespaces fork their view of the given-set.
- Two namespaces can disagree about whether the same hash is a given.
  This is a feature (per ADR-005's "namespace = instance set") but
  means UCM commands must always read givenness through the current
  namespace, never by hash alone.
- `MdValues` is currently a write-mostly schema slot. Phase 2.B must
  thread metadata reads through name lookup, project APIs, view/find,
  and any command that surfaces definitions. `grep MdValues unison-cli`
  is empty today; this is real plumbing work.
- ADR-020 inherits the storage decision (revive `causal_metadata`,
  reuse `MdValues` with a sentinel `##Builtin.Given`, or new field).
- ADR-021 inherits the merge story: namespace-metadata conflicts during
  `unison-merge` need a defined resolution.
- Sharing protocol (`unison-share-api`) must transmit the metadata
  field; an API version bump is required and a coordinated server
  release is required before client release.
