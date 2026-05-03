# ADR-016: Update semantics on re-resolution

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-002, ADR-005, ADR-014, ADR-015, ADR-017; `docs/implicits-plan.md` §5.6, §10 (risk: "Update semantics surprise users")

## Context

`update` re-elaborates an edited term against the current namespace. If
the visible given-set changed since the last elaboration — a new given
appeared, an existing one was removed, or one was renamed — the
elaborator may now resolve to a *different* dictionary than before. The
old term retains its old hash and old resolution (content addressing is
preserved by ADR-002 principle 2), but the new term has a different hash
and different runtime behavior than the user might expect.

Three scenarios matter:

1. **Resolution unchanged.** Re-elaboration picks the same dictionary;
   no semantic change; everything proceeds as ordinary update.
2. **Resolution changed but unambiguous.** The new given-set exposes a
   different unambiguous winner. The new term hashes differently. The
   user may or may not have anticipated this.
3. **Re-elaboration becomes ambiguous or fails.** The new given-set
   surfaces two equally specific candidates, exceeds the depth limit, or
   no candidate matches. The update cannot complete; we must report.

## Options considered

### Option A: Fail loud on any resolution change

Treat (2) and (3) the same: any change in the resolved dictionary set
aborts the update with an elaborator error listing the old and new
resolution. Pros: zero surprises; users opt in explicitly. Cons:
extremely chatty in normal workflows where users intentionally added a
new given so it would take effect; punishes the mainline use case;
arguably contradicts the design intent that "namespace = instance set"
means changing the namespace changes resolution.

### Option B: Silently re-resolve and produce a new hash

Treat (2) as ordinary `update` semantics — new hash, new behavior, old
code unaffected. Treat (3) as `update` failure (NoGiven, Ambiguous, or
DepthExceeded) with structured error. Pros: matches the rest of `update`
semantics (content addressing already means old dependents see old
hash); aligns with the "resolution is part of the hash" principle;
mainline workflow is uninterrupted. Cons: a user who changes a given
unintentionally may not notice the resolution flipped; mitigated by the
diff that `update` already presents.

### Option C: Require an explicit `--allow-resolution-change` flag

Default to refusal; require an explicit opt-in flag for any update that
would change resolution. Pros: forces the user to acknowledge the
change. Cons: as chatty as Option A in practice; flag fatigue;
indistinguishable from Option A in most workflows.

## Decision

We recommend **Option B,** pending ratification:

- For ambiguity-free, successful re-elaboration: silently re-resolve and
  produce the new hash. The old term retains its old hash and its old
  dependents. This is consistent with how every other kind of `update`
  already works.
- For re-elaboration that becomes ambiguous, exceeds depth, or has no
  matching given: fail loud with a structured elaborator error
  (`NoGiven`, `Ambiguous`, `DepthExceeded`) and abort the update.

The `update` diff display already shows old vs new hash; the verbose
printer mode (ADR-015) can show the chosen dictionaries when the user
wants to inspect the change.

## Consequences

- **Unblocks** Phase 2.E exit criterion: "(a) working update through a
  given changes hash; (b) breaking a given leaves dependent terms valid
  but breaks new code; (c) introducing a second matching given causes
  new code to fail with ambiguity but doesn't disturb existing code."
- **Depends on** ADR-005 (ambiguity reporting machinery), ADR-009 (depth
  errors), and ADR-014 (so the new hash is well-defined).
- **Influences** ADR-015: an elide-mode pretty-printed term that
  re-elaborates differently after a given-set change is exactly the
  scenario this ADR governs; the printer must not silently drop
  information that turns case (3) into case (2).
- Required tests: golden hash round-trips for cases 1, 2, 3; an update
  transcript showing a resolution flip with both old and new hash
  visible in the diff; structured error fixtures for the three failure
  categories; a regression test that breaking a given (deleting it)
  leaves dependent terms hashable and runnable but breaks new code at
  the same site.
- Adds a documentation obligation: the migration guide (Phase 6) must
  explain why `update`-after-given-edit can flip resolution and how to
  inspect the diff.
