# ADR-013: Namespace given-set storage format

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-002, ADR-014, ADR-020, ADR-021; `docs/implicits-plan.md` §5.2

## Context

Per ADR-002, "givenness" is a tag attached to a hash inside a namespace,
not part of the term itself. We need a concrete storage shape for the
per-namespace set of given-tagged hashes. The choice has to fit the
existing codebase model in
`codebase2/codebase/U/Codebase/Branch/Type.hs:32`, which already records
`MdValues = Set MetadataValue` per `(NameSegment, Referent)` for the
existing metadata feature.

The decision interacts with serialization (ADR-020), branch hashing
(ADR-014), merge semantics (ADR-021), and migration. SQLite already has
a dormant `causal_metadata` table at
`codebase2/codebase-sqlite/sql/create.sql:113` that points at a different
shape — per-causal rather than per-name — and is currently unused.

Three storage options exist; each affects schema churn, migration cost,
and how the merge code in `unison-merge/src/` will need to change.

## Options considered

### Option A: Reuse `MdValues` with a sentinel `Reference`

Tag a name with given-ness by inserting a sentinel built-in `Reference`
— call it `##Builtin.Given` — into the existing
`Map Referent (m MdValues)` in `Branch m`. The schema, serialization,
hashing, and read/write paths already exist; givenness becomes "a known
metadata reference whose presence is significant." Pros: zero schema
changes; piggy-backs on a slot already plumbed through codebase
serialization (`unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs`
already includes `MdValues` in branch tokens, so causal hashing
automatically tracks the change); aliases preserve the metadata
naturally. Cons: needs every read path that surfaces metadata to know to
filter or recognize the sentinel; the dormant `causal_metadata` table
remains dormant; metadata semantics are stretched slightly (a sentinel
is a flag dressed up as a value).

### Option B: Revive the dormant `causal_metadata` table

Use the existing-but-unused per-causal metadata table at
`codebase2/codebase-sqlite/sql/create.sql:113` to record givenness keyed
on causal id. Pros: gives metadata a real home rather than reusing a
sentinel; the schema slot was designed for something like this. Cons:
shape mismatch — `causal_metadata` is per-causal, but givenness is
per-`(NameSegment, Referent)` within a branch; we would need a
secondary lookup or a different schema; significantly more migration
work; breaks the principle of fitting through the smallest possible
hole.

### Option C: New field on branch serialization

Extend `U.Codebase.Branch.Type.Branch` with a new top-level `Set
Referent` (or `Map NameSegment (Set Referent)`) recording givens. Pros:
explicit, self-documenting, easy to read. Cons: invasive — touches
serialization, hashing token list, sync/push, and every migration; new
codebase format version; coordinated server release.

## Decision

We recommend **Option A: reuse `MdValues` with a sentinel
`##Builtin.Given` reference,** pending ratification. The choice
minimizes schema churn, exploits the metadata pathway that already
participates in branch hashing, and matches the design principle
(ADR-002) that givenness is a namespace-level tag. The dormant
`causal_metadata` table stays dormant; ADR-020 makes that explicit.

## Consequences

- **Unblocks** Phase 2.B (namespace given-set storage) — work proceeds
  against the existing `MdValues` slot rather than a new schema.
- **Depends on** ADR-014 (which confirms branch-hash semantics for
  metadata changes) and ADR-020 (which formalizes the on-disk encoding
  and sharing-protocol bump).
- **Forces** ADR-021 to teach `unison-merge/src/` how to diff and merge
  the sentinel within `MdValues`; today the merge code has no
  metadata-aware logic.
- Sentinel choice locks us out of using `##Builtin.Given` for any other
  purpose; document the reservation in the builtins module.
- All read paths that surface metadata (UCM `find`, `view`, project APIs,
  Share APIs) must filter or special-case the sentinel; failing to do so
  exposes "Given" as a phantom metadata value to users.
- Required tests: round-trip of a namespace through SQLite with
  givens preserved; alias copies the sentinel; rename preserves it; a
  migration test asserting existing namespaces have empty given-sets;
  branch-hash determinism across mark/unmark.
