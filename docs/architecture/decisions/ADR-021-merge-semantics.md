# ADR-021: Merge semantics for given-set conflicts

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-002, ADR-013, ADR-017, ADR-020; `docs/implicits-plan.md` §5.2

## Context

`unison-merge/src/` performs three-way namespace merges. Today it has
*no* metadata-aware logic: `grep -rn 'MdValues\|metadata' unison-merge/src/`
returns nothing. Givens add a per-name tag (per ADR-013, encoded as the
`##Builtin.Given` sentinel inside `MdValues`), so merges must learn how
to combine the tag across LCA, "alice," and "bob" branches.

Four cases must be defined explicitly. Each maps to either an existing
merge code path with metadata threading added, or a new code path.

## Options considered

The cases below are not "options" in the usual sense — every merge
implementation must decide each case. We list the cases and the chosen
resolution; an Option A/B presentation would be artificial.

### Case (a): Both branches mark the same hash given

Alice's branch and Bob's branch both add the `##Builtin.Given` sentinel
to the same `(NameSegment, Referent)`. Expected result: no-op. The
merged namespace has the tag. No user prompt.

### Case (b): One branch marks, the other does not

Alice marks; Bob did not (or unmarked). LCA either had it or didn't.
Resolution: **mark wins by default, user-confirmable.** Promoting a
definition to a given is a strictly additive operation in the design
(downstream users can demote locally per ADR-002 principle 4); a merge
that drops a deliberate mark is more surprising than one that keeps it.
The user gets a confirmable summary entry: "Alice marked Foo as given;
Bob did not. Keep mark? [Y/n]." Non-interactive merges (CI) default to
Y.

### Case (c): Both branches rename the same given to different names

Alice renames `Show.nat` to `ShowNat`; Bob renames it to `Show.Nat`.
This routes through the existing rename-conflict path in
`unison-merge/src/`. The givenness *travels with the hash*: whichever
name resolution the user picks, the `##Builtin.Given` sentinel
accompanies it. No new conflict category — the rename-conflict UI
already prompts; we just preserve the metadata.

### Case (d): One branch marks, the other deletes the definition

Alice marks `Show.nat` as a given; Bob deletes `Show.nat`. This routes
through the existing delete-conflict path (modify-vs-delete is already
a known conflict category). Decision rule: same as Case (b) for the
mark, but layered on top of the existing delete conflict. The user is
prompted to either accept the deletion (the mark is moot) or restore
the definition with the mark applied. No new conflict category.

## Decision

We recommend, pending ratification, that `unison-merge` add metadata
awareness with these specific resolutions:

| Case | Resolution                                                   |
|------|--------------------------------------------------------------|
| (a)  | No-op: mark survives.                                        |
| (b)  | Mark wins by default; user-confirmable; CI default Y.        |
| (c)  | Existing rename-conflict path; givenness travels with hash.  |
| (d)  | Existing delete-conflict path; mark is preserved if the definition is restored. |

## Consequences

- **Unblocks** Phase 2.B exit criterion: "merge with given-set
  conflicts" round-trip tests.
- **Depends on** ADR-013 (storage shape determines what the merge code
  reads) and ADR-020 (sentinel reservation).
- **Forces** `unison-merge/src/` to grow metadata-aware diffing —
  specifically `unison-merge/src/Unison/Merge/Diffblob.hs`,
  `Diff.hs`, and `CombineDiffs.hs` need to project `MdValues` into the
  diff representation, and `PartitionCombinedDiffs.hs` needs to
  recognize the four cases above.
- Adds a UCM prompt category for case (b) and a layered prompt for
  case (d); ADR-017's `merge` command surface must show the prompts.
- Required tests: transcript fixtures for each of (a)–(d); a regression
  test that today's metadata-free merges still produce identical
  results when no givens are involved (i.e. the new code path is a
  no-op when `MdValues` is empty); a CI-mode test confirming default-Y
  for (b) and the documented default for (d).
- Locks "mark wins" as the default policy; reversing it later is
  user-visible and would need a deprecation pass.

## Open questions

- Whether case (b) should default to *no prompt* in CI but *prompt* in
  interactive mode (current recommendation), or always prompt and have
  CI configure a non-interactive answer. Mostly a UX choice; flag for
  ratification.
- Whether the existing `unison-merge` types (`TwoWay`, `ThreeWay`,
  `EitherWay`, etc.) need new variants to express "metadata change" or
  whether metadata diffs ride alongside as a parallel structure. The
  parallel-structure approach is cheaper to land first and is the
  starting recommendation; document if the spike forces variants.
