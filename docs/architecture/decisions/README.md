# Architecture Decision Records

This directory holds Architecture Decision Records (ADRs) for substantial,
hard-to-reverse design choices in the Unison implementation.

## Format

Each ADR is one file: `ADR-NNN-short-title.md`. Use the template in
[`TEMPLATE.md`](TEMPLATE.md). Status values:

- **Proposed** — drafted, not yet ratified
- **Accepted** — ratified by core maintainers; consequences are committed
- **Deprecated** — superseded; see "Replaced by"
- **Rejected** — considered and explicitly rejected

## Index

### Implicit parameters (`givens`)

The plan that motivates ADRs 001–022 lives in
[`docs/implicits-plan.md`](../../implicits-plan.md).

| #   | Title                                                       | Status   |
|-----|-------------------------------------------------------------|----------|
| 001 | [Constraint syntax: Haskell-style `C =>`](ADR-001-constraint-syntax.md) | Accepted |
| 002 | [Givenness as namespace metadata](ADR-002-givenness-namespace-metadata.md) | Accepted |
| 003 | [No new "class" declaration kind](ADR-003-classes-are-ordinary-types.md) | Accepted |
| 004 | [Resolution is compile-time only](ADR-004-compile-time-resolution.md) | Accepted |
| 005 | [Coherence: per-call-site ambiguity](ADR-005-coherence.md)  | Accepted |
| 006 | [`summon T` as the explicit summon form](ADR-006-summon-syntax.md) | Accepted |
| 007 | [`@`-positional explicit override](ADR-007-explicit-override.md) | Accepted |
| 008 | [Specificity ordering](ADR-008-specificity.md)              | Accepted |
| 009 | [Cycle detection and depth limit](ADR-009-cycles-and-depth.md) | Accepted |
| 010 | [Local givens scoping](ADR-010-local-givens.md)             | Accepted |
| 011 | [HKT in v1; FDs/assoc-types deferred](ADR-011-hkt-scope.md) | Accepted |
| 012 | [Elaborator pass placement vs inference](ADR-012-elaborator-placement.md) | Proposed |
| 013 | [Namespace given-set storage format](ADR-013-given-set-storage.md) | Proposed |
| 014 | [Hashing of givens and elaborated terms](ADR-014-hashing.md) | Proposed |
| 015 | [Pretty-printer default behavior](ADR-015-printer-default.md) | Proposed |
| 016 | [Update semantics on re-resolution](ADR-016-update-semantics.md) | Proposed |
| 017 | [UCM command surface](ADR-017-ucm-surface.md)               | Proposed |
| 018 | [Feature-flag rollout](ADR-018-feature-flag.md)             | Proposed |
| 019 | [Implicit-arrow representation in `Type.F`](ADR-019-type-f-representation.md) | Proposed |
| 020 | [Metadata serialization and `causal_metadata` migration](ADR-020-metadata-serialization.md) | Proposed |
| 021 | [Merge semantics for given-set conflicts](ADR-021-merge-semantics.md) | Proposed |
| 022 | [Keyword migration plan](ADR-022-keyword-migration.md)      | Proposed |
| 023 | [Memoization scope and cache key](ADR-023-memoization-scope.md) | Proposed |
