# Phase 1 spike — findings

This is the Phase 1 gate writeup required by `docs/implicits-plan.md` §4.4.

## Status

All six spike success criteria from `docs/implicits-plan.md` §4.2 are
demonstrated. 15 test cases under `tasty` + `tasty-hunit`, all passing on
GHC 9.10.3.

```
implicit-resolution spike
  1. cycles terminate                                  2/2 OK
  2. specificity ordering                              2/2 OK
  3. HKT support                                       3/3 OK
  4. ambiguity                                         1/1 OK
  5. performance (1000 givens, depth-20 chain) <2s     1/1 OK
  6. diamond dependencies                              2/2 OK
  sanity                                               4/4 OK
```

Performance: 1000-given pool with depth-20 chain resolves in
sub-millisecond. Dominant cost is the O(n) head-unification scan over
candidates; a conclusion-keyed index would fix it if it ever matters in
production.

## What worked

- The `docs/implicits-plan.md` §1.3 algorithm translated almost
  line-by-line into ~280 lines of Haskell. One `State` monad threads the
  fresh-supply, memo, and work counter.
- HKT works for free with curried `TApp`; no kind machinery needed in
  the resolver. Matches ADR 011's claim.
- Per-branch cycle detection via in-flight goal stack handles
  `Foo<->Bar` mutual recursion without hanging; sibling alternatives
  still succeed.
- Memoization keyed on the substitution-applied goal, scoped to one
  top-level `resolve`, decisively handles diamond dependencies.
  Instrumented `resolveCounted` confirms `Show (Map (List Nat) Nat)`
  solves exactly 3 distinct sub-goals (top, `Show (List Nat)`, `Show
  Nat`).

## Surprises

- **Specificity must compare *un-specialised* (alpha-renamed but not yet
  unified) conclusions.** First pass compared the post-unification
  conclusions, but those equal the goal by construction — they
  head-unified with it. Fix was to retain `candFreshConclusion` on each
  candidate. *Plan implication: ADR 008 wording is ambiguous on this
  point — see refinement below.*
- Cycle hits use `unify`, not `==`, to catch alpha-renamed cycles.
  Correct, but it can spuriously fire when an outer goal has unresolved
  metavars. Warrants a property test in Phase 2.D.
- Candidate freshening must happen on *every* match attempt, before
  head-unification, to avoid one candidate inheriting variable bindings
  from another.
- `tasty` was zero-friction. `easytest` would have been fine but
  `tasty`'s grouped streaming output suited criterion-by-criterion
  validation.

## What the plan should refine

1. **Tighten ADR 008 (specificity).** "σ(B) = A and σ ≠ identity" is
   ambiguous about (a) whether to compare declared or specialised
   conclusions and (b) which side's variables σ may touch. The right
   rule is: compare alpha-renamed *declared* conclusions, with σ only
   allowed to substitute B's quantified variables (A's are treated as
   rigid). The spike's `oneWayMatch` does this; full `unify` would be
   too permissive and reverse the asymmetry.

2. **Add ADR 023 — memoization scope and cache-key shape.** §4.2#6
   mentions memoization as a property but doesn't specify what gets
   memoized (failures? successes? both?), nor how cache keys handle
   metavariables. Recommendation: per top-level resolve, key = zonked
   goal type, cache successes and failures alike. Sub-goals containing
   free metavars (which they will, before TDNR pins types down) need an
   explicit story.

3. **`NearMiss` should carry the unifying substitution**, not just the
   given. "Near-miss: `Show.list` with `a := Nat`" is more useful than
   just `Show.list`.

4. **Extend `ResolveError` with `Cycle [Ty]`.** The §1.3 pseudo-code
   says "fail this branch" but doesn't specify the error variant. The
   spike used `NoGiven goal []`, which technically works but conflates
   "no instance" with "instance exists but cyclic". Phase 2.D should
   distinguish these for diagnostic clarity.

## Phase 2 readiness

No ADR-level showstoppers. Data model translates cleanly:

| Spike type           | Phase 2 home                                                  |
|----------------------|---------------------------------------------------------------|
| `Ty`                 | `Unison.Type.Type v loc`                                      |
| `Given`              | record keyed on dictionary `Reference`                        |
| `Pool`               | snapshot from the threaded lexical-given-environment (2.C.2)  |
| `ResolutionTree`     | fed into an `applyTdnrDecisions`-shaped post-pass (2.D)       |
| `ResolveError`       | structured error notes consumed by the existing renderer (4)  |

Recommend opening **ADR 023** and tightening **ADR 008** before Phase 2
kickoff. Both are clarifications, not direction changes.

## Reproducing

```sh
cd spike/implicits
cabal test
```

Requires GHC 9.10.3 (matches the rest of the repo per `stack.yaml`'s
LTS-24.21).
