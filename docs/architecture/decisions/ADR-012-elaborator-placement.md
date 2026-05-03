# ADR-012: Elaborator pass placement vs inference

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-004, ADR-013, ADR-014, ADR-019; `docs/implicits-plan.md` §5.4, §5.5

## Context

Implicit-parameter elaboration must turn typechecker-recorded "constraint
goals" — `(SourceLocation, Type, ScopeSnapshot)` triples — into concrete
`App` nodes filled in with resolved dictionaries. The question is *when*
this pass runs in the existing pipeline.

The pipeline today already has a similar shape for type-directed name
resolution (TDNR). `typeDirectedNameResolution` in
`parser-typechecker/src/Unison/Typechecker.hs:262` runs a fixed-point loop
over `InfoNote`s, and `applyTdnrDecisions` in
`parser-typechecker/src/Unison/FileParsers.hs:329` walks the term and
substitutes the chosen `Term v` for each recorded `Decision`. We can
either fit implicit elaboration into the same shape or pick a different
position in the pipeline.

Inference itself lives in
`parser-typechecker/src/Unison/Typechecker/Context.hs` (~3800 LoC of
bidirectional inference). Whatever placement we choose has to interact
cleanly with how that file threads its state.

## Options considered

### Option A: One-shot post-pass after TDNR's fixed point

Run inference (with TDNR's fixed-point loop) to completion. The
typechecker emits `InfoNote`s recording each implicit goal — type, scope
snapshot, and a fresh `Implicit` blank in the term. Once TDNR reaches its
fixed point, a single elaborator pass walks the goal list, runs
resolution per ADR-008/009, and substitutes the chosen dictionary terms,
mirroring `applyTdnrDecisions`. Pros: cheapest to implement, fits the
existing extension pattern, no new fixed-point machinery. Cons: if
implicit resolution itself produces a term whose type still has
unresolved metavariables, we cannot feed those back into TDNR — but the
spike (Phase 1) and the design contract (constraint goals carry pinned
types) make this case rare and addressable with a localized re-check.

### Option B: Interleaved within TDNR's fixed-point loop

On each TDNR iteration, also resolve any newly added implicit goals;
re-synthesize the type; iterate. Pros: handles the rare case where an
implicit's resolution exposes a new TDNR opportunity (or vice versa).
Cons: doubles the cost of the inner loop; couples two independently
designed mechanisms; harder to debug when one of them oscillates.

### Option C: Run before inference

Resolve implicits at parse-to-AST time, before typechecking. Pros: keeps
the typechecker unaware of implicits. Cons: untenable — resolution
*requires* the inferred types of holes to know what to look up. The whole
point of given resolution is type-directed.

## Decision

We recommend **Option A: a one-shot post-pass after TDNR's fixed point,**
pending ratification. The pass mirrors `applyTdnrDecisions` and reuses
the same `InfoNote`/`Decision`/blank-substitution shape. This is the
minimum viable mechanism that fits the codebase, and the spike will
confirm whether the rare TDNR-after-implicit case appears in practice. If
it does, we can promote the pass into the TDNR fixed point as a follow-up
without invalidating any consumer.

## Consequences

- **Unblocks** Phase 2.D (elaborator implementation) — the resolver lifts
  out of the spike with a known place to be plugged in.
- **Depends on** ADR-019 (so the typechecker can recognize implicit
  arrows during inference), ADR-013 (so the pass has a given-set to read
  from), and ADR-014 (for hashing the elaborated term).
- ADR-016 (update semantics) presupposes deterministic, post-pass
  elaboration: an `update` re-runs inference plus the post-pass and
  compares hashes.
- The elaborator output must be deterministic given the same goal list
  and given-set; locked with golden-hash tests in Phase 2.E.
- Required tests: (a) implicit goals appear and are resolved against
  current TDNR test fixtures with no regressions; (b) a
  TDNR-then-implicit case where TDNR pinning the type unlocks resolution;
  (c) a "resolution surfaces a new TDNR candidate" case to confirm
  whether Option B becomes necessary later.
- Should the spike force Option B, the change is internal: only the
  loop's driver in `Typechecker.hs` moves; ADRs 013/014/019 are
  unaffected.
