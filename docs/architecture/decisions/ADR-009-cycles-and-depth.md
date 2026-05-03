# ADR-009: Cycle detection per branch; depth limit global, default 50

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-008; `docs/implicits-plan.md` §1.3 ("Cycle and depth handling")

## Context

Implicit resolution is a recursive search: a given for `Show (List a)`
has a premise `Show a`, which must itself be resolved, and so on. Two
things can go wrong with that search.

First, **cycles.** A user can declare `given Foo : C => C` (or some
chain of givens that loops). Naïve recursive search would diverge.
Second, **explosion.** Even without cycles, a chain of premises can be
deep (think `Show (Map (List (Optional Nat)) [Text])`), and pathological
cases can blow up the search tree exponentially.

The resolver needs defenses for both, and the defenses must distinguish
"this branch is bad, try another candidate" (recoverable) from "the
whole search is doomed" (hard error).

## Options considered

### Option A: Per-branch cycle detection + global depth limit (default 50, configurable)

While exploring a candidate `g`, maintain a `stack` of types-being-resolved
on this branch. If a sub-goal `T` unifies with anything on `stack`, fail
*this branch* (try other candidates). Independently, maintain a global
counter `depth`; if it exceeds a configured limit (default 50,
overridable via project setting), error out the whole resolution with
`DepthExceeded`, reporting the chain. Pros: cycles per-branch let
alternative candidates succeed even when one alternative loops; depth
limit catches non-cyclic explosions; `DepthExceeded` errors are
actionable (the chain points at the offending givens); the algorithm
matches the pseudocode in `docs/implicits-plan.md` §1.3 directly. Cons:
two mechanisms instead of one; the depth bound is arbitrary and might
need tuning.

### Option B: Depth-only with no cycle detection

Just rely on the depth limit; cycles will hit it eventually. **Rejected.**
The error becomes "depth exceeded" for what is actually a structural
cycle, masking the real bug. The chain in the error wouldn't reveal
the loop because depth would terminate the search before the loop
closed visibly. Diagnostics suffer.

### Option C: Cycles as hard failure of the entire search

If any sub-goal cycles, fail the whole top-level resolution with
`Cycle` rather than failing only that candidate. **Rejected.** Kills
valid alternative branches. Example: candidate `g1` cycles on its
premise but candidate `g2` resolves cleanly — Option A picks `g2`,
Option C errors. The latter is strictly worse.

## Decision

We adopt **Option A: per-branch cycle detection plus a global depth
limit.** Cycles fail only the candidate that hit them; depth limit
errors out the whole resolution with the chain. Default depth is 50,
configurable via a project-level setting.

## Consequences

- The resolver maintains two pieces of state: a per-branch type-stack
  (for cycle detection) and a global depth counter (for explosion
  control). Both are threaded through the recursive `resolve` call,
  exactly as shown in `docs/implicits-plan.md` §1.3.
- Cycle detection is by *unification* against the stack, not by hash
  identity. Two textually-different goals that unify to the same type
  count as a cycle — this catches more loops than naive equality.
- The default of 50 is generous for honest code (typical chains in
  the spike test corpus are <10 deep) but small enough to terminate
  pathological inputs in milliseconds. Users can raise it via a
  project setting if they have a legitimate deep-chain use case.
- Three structured error categories from §7.2: `NoGiven`,
  `Ambiguous`, `DepthExceeded`. Cycles do *not* surface as a
  user-visible error category — they are invisibly handled by
  per-branch failure. If a cycle prevents *any* candidate from
  succeeding, the user sees `NoGiven` (with near-misses including the
  cycling candidate, ideally).
- The spike (Phase 1) must demonstrate cycle termination and depth
  behavior as success criteria.
- `DepthExceeded` errors print the chain (truncated sensibly) and
  mark the recursive step. Phase 4 has a golden test for this case.
- Locks us into "no proof-search backtracking beyond what
  per-candidate try-catch provides." Adding more sophisticated search
  (e.g., dependent constraint solving) would require revisiting
  this.
