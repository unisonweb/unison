# ADR-023: Memoization scope and cache key for given resolution

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-008 (specificity), ADR-009 (cycles and depth);
`docs/implicits-plan.md` §1.3, §4.2#6, `spike/implicits/FINDINGS.md`

## Context

The Phase 1 spike validated `docs/implicits-plan.md` §4.2 success
criterion #6 — "diamond dependencies don't blow up" — by memoizing
sub-resolutions within a single top-level `resolve`. That worked, but
the plan's text (§4.2#6 and §1.3) does not specify:

1. **Scope.** Is the memo per-call-site (one top-level resolve), or
   per-elaboration-pass (across all call sites in a file), or global?
2. **Key.** Is the key the raw goal type, the substitution-applied
   ("zonked") goal, or something else? How are free metavariables
   treated?
3. **What gets cached — successes only, failures only, both?**

These are not bikesheds: getting them wrong produces either exponential
blowup (the failure mode the spike's criterion #6 was meant to guard
against) or incorrect resolutions (a sub-result reused in a context
where the metavar bindings have changed).

The spike's choice was "per top-level resolve, key = zonked goal,
successes only," and it passed all 15 tests including the diamond cases.
But the spike doesn't have free metavars from the *outer* type checker —
in production, sub-goals will frequently contain metavars that are not
yet pinned, since elaboration runs after TDNR but TDNR may itself emit
goals containing existentials.

## Options considered

### Option A: Per top-level resolve, zonked key, cache both successes and failures

Each invocation of `resolve T` allocates a fresh memo table. Keys are
the substitution-applied (zonked) goal type. Both successful resolutions
and `NoGiven`/`Cycle`/`DepthExceeded` outcomes are cached. The table is
discarded when `resolve T` returns.

**Pros:** matches what the spike implemented; clearly bounded memory
(no cross-call-site leakage); caching failures is critical for the
diamond case where the same dead-end is reached repeatedly. **Cons:**
sub-goals containing un-pinned metavars hash to the same key as
*different* sub-goals that happen to share the metavars — risk of
false-positive cache hits if metavars are shared across the resolve.

### Option B: Per elaboration-pass, zonked key

A single memo table for the whole pass. Pros: more reuse across call
sites in the same file. Cons: lifetime of cached resolutions outlives
the metavar context they were resolved against. Risk of incorrect reuse
when a later call site re-uses a key whose underlying metavars differ.
Mitigation would require storing the metavar context with each cache
entry, which approaches the cost of just re-resolving.

### Option C: Per top-level resolve, key includes metavar context

Like A, but the cache key is `(goal, snapshot of relevant metavar
bindings)`. Pros: handles shared metavars correctly. Cons: complicated
key; metavar-equivalence becomes a non-trivial computation; risk of
making every cache lookup slower than the work it saves.

### Option D: No memoization, accept O(2^n) on diamonds (rejected)

Simplest. **Rejected** — fails plan §4.2#6.

## Decision

We recommend **Option A: per top-level resolve, zonked goal as key,
cache successes and failures alike**, with the explicit qualification
that the cache is **invalidated whenever a metavariable in the in-flight
goal stack is bound** during sub-resolution. (In practice this means: if
two sub-goals share a metavar and one resolution binds it, the other
sub-goal's cached result is dropped.)

This is the cheapest design that's correct for the diamond cases and
honest about the metavar interaction.

## Consequences

- **Phase 2.D implementation:** the elaborator's resolver carries a
  `Map ZonkedTy CachedResolution` allocated fresh per top-level resolve.
  Cache is dropped when any metavar in the goal stack is bound; cheaper
  than recomputing equivalence classes for each lookup.
- **Phase 2.D test matrix:** add a property test for diamond resolution
  with shared metavars across the diamond's two paths. The spike
  doesn't cover this — its goals were ground.
- **Performance:** memory is O(distinct sub-goals in one resolve), which
  is small in practice. Time savings on diamonds are substantial: the
  spike measured `Show (Map (List Nat) Nat)` solving exactly 3 distinct
  sub-goals; without memoization, the same diamond would be O(2^depth).
- **Failure caching is load-bearing.** Without it, a failed sub-goal
  reached from N parent paths costs N × (full failure-search cost) —
  enough to dominate runtime on realistic instance pools.
- **Locks the language out of cross-call-site memoization** without a
  follow-up ADR. Future work on speeding up large files (where many
  call sites resolve the same constraints) may want to revisit this; if
  so, Option C with a proper metavar-context key is the path forward.
- **Open question for Phase 2.D:** the exact metavar-binding
  invalidation rule. The recommendation here is "any binding within the
  goal stack drops the cache," but a finer-grained rule may be possible
  ("only drop entries whose key contains the just-bound metavar"). Test
  matrix should include a benchmark distinguishing the two.
