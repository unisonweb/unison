# ADR-020: Metadata serialization and `causal_metadata` migration

**Status:** Proposed (foundational, blocking)
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-002, ADR-013, ADR-014, ADR-021; `docs/implicits-plan.md` §3.3 (gate),
§5.2

## Context

Givenness lives in namespace metadata (ADR-002). The codebase has *two*
plausible homes for that metadata:

- `MdValues = Set MetadataValue` per `(NameSegment, Referent)` defined
  at `codebase2/codebase/U/Codebase/Branch/Type.hs:32-43`. Already
  serialized; already participates in branch-hash tokens via
  `unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs`; already round-tripped through SQLite.
- `causal_metadata` table at `codebase2/codebase-sqlite/sql/create.sql:113`. Defined in the schema, currently unused, keyed on
  `causal_id × object_id × component_index`.

Sharing makes the question concrete: any change to the metadata wire
format requires bumping the protocol version in `unison-share-api` and
`unison-share-projects-api`, plus a coordinated server release. We must
pick *one* home and version it.

ADR-013 already tentatively chose Option A (reuse `MdValues` with a
sentinel `##Builtin.Given`). This ADR formalizes the on-disk
serialization decision and disposes of the dormant table.

## Options considered

### Option A: Reuse `MdValues` (preferred)

Encode given-ness as a sentinel `MetadataValue` (`##Builtin.Given`)
inside the existing `MdValues` slot. No schema change. Wire-format
change is *additive*: clients that don't know the sentinel see it as an
opaque metadata reference and round-trip it through unchanged; clients
that do know it interpret it as a tag. Pros: minimal churn; piggy-backs
on serialization already used by sync; per-name resolution matches the
shape of the data (givenness is per-name, not per-causal); branch
hashing already includes it. Cons: requires a careful documentation that
`##Builtin.Given` is a reserved metadata reference; the dormant
`causal_metadata` table remains dormant and should be marked as such in
the SQL.

### Option B: Revive `causal_metadata`

Use the `causal_metadata` table for given-set storage. Pros: gives the
dormant table a purpose. Cons: shape mismatch — the table is keyed on
causal id, but givenness is per `(NameSegment, Referent)`; a secondary
table or column would be needed; new wire format; a new sync code path;
client and server must both upgrade in lockstep without an additive
fallback.

### Option C: New schema

Add a fresh table and a fresh wire field for given metadata. Pros:
self-documenting. Cons: invasive — new SQL migration, new sync
serialization, full protocol bump, no fallback for older clients.

## Decision

We recommend **Option A,** pending ratification, with these specifics:

- The given-tag is a sentinel `MetadataValue` reference,
  `##Builtin.Given`, recorded in `MdValues` for each given-tagged
  `(NameSegment, Referent)`.
- The `causal_metadata` SQL table is documented as deprecated/reserved
  but **not dropped** in the same release; dropping it is a separate
  schema-cleanup change with its own ADR if desired.
- The Sharing wire format admits the addition of `##Builtin.Given` as
  an opaque metadata reference with **no protocol version bump**. This
  reverses an earlier draft of this ADR. The chunk B3 investigation
  confirmed: `putLocalBranch`/`getLocalBranch` encode metadata as a
  generic `Set Reference` (`putMetadataSetFormat = putWord8 0 *>
  putFoldable putReference`), `putReference (ReferenceBuiltin t) =
  putWord8 0 *> putVarInt t` is fully generic over the textId, and
  SyncV2's `StreamInitInfo` is future-compatible at the schema level
  (per its `Serialise` instance). Bumping the hard-coded `Version 1` in
  `Unison.Share.SyncV2` would surface `SyncErrorUnsupportedVersion`
  against existing servers — a breaking change for an additive feature
  is the wrong tradeoff.
- **Discoverability and feature gating** (which the earlier draft
  called out as the reason to bump) instead use a
  `StreamInitInfo`-style map-key entry like `"implicits"` introduced at
  Phase 3 entry, since that struct is already future-compatible without
  a version bump.
- No coordinated server release is required for B-track delivery; the
  sentinel travels through unmodified servers transparently.

## Consequences

- **Unblocks** Phase 2.B (storage subproject) and by transitivity every
  later phase that needs to push/pull/merge given-sets.
- **ADRs depending on this one:** 013 (storage shape), 014 (branch-hash
  semantics rely on `MdValues` participating in tokens), 021 (the merge
  code reads metadata from `MdValues`), 017 (UCM commands flow through
  the same metadata path).
- Locks `##Builtin.Given` as a reserved metadata reference; record this
  in the builtins module and flag in any future metadata-feature
  discussion.
- Forces the read paths that today ignore metadata
  (`grep -rn MdValues unison-cli` returns nothing per the plan §5.2) to
  start consulting it. Implementation lands in
  `unison-cli/src/Unison/Codebase/Editor/HandleInput/`.
- Required tests: round-trip a namespace through SQLite and back with
  givens preserved; round-trip across the Sharing API (no version
  change required); an old-client/new-server compatibility test is
  *not* required because the wire format already carries arbitrary
  metadata references opaquely.
- Sharing protocol: we incur a permanent obligation to negotiate
  protocol version on connect; library code that relies on a specific
  version must read the negotiated version, not assume one.

## Open question

Whether to ever revive `causal_metadata` for some other purpose, or
schedule its eventual drop. Out of scope here; flag for a future
schema-cleanup ADR.
