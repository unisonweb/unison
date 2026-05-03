# ADR-017: UCM command surface

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-013, ADR-015, ADR-016, ADR-018, ADR-021; `docs/implicits-plan.md` §6

## Context

UCM commands live in
`unison-cli/src/Unison/CommandLine/InputPatterns.hs` (a flat ~4671-line
table). Adding givens means (a) introducing commands that have no
analogue today (toggling the namespace tag, listing givens) and
(b) extending existing commands so that givenness is visible and
preserved through every namespace operation.

Implementations of these commands live in
`unison-cli/src/Unison/Codebase/Editor/HandleInput/`, which is also
where the `MdValues` plumbing must reach to honor ADR-013.

## Options considered

### Option A: Distinct `mark.given` / `unmark.given`, plus a `givens` lister; modify the natural surface of namespace commands

**New commands.**

- `givens [path]` — lists every given-tagged definition at `path`,
  showing name, hash prefix, and inferred conclusion type. Mirrors
  `find` in shape but filters to the given set.
- `mark.given <name>` — toggles the namespace tag *on* without rehashing
  the term. Per ADR-014, the term hash is unchanged; the branch hash
  changes because `MdValues` participates in tokens.
- `unmark.given <name>` — toggles the tag *off* with the same
  hash-stability properties.

**Modified commands.**

- `view` — when the target is given-tagged, prepend the `given` keyword
  in the printed form (Phase 4 wires this into the pretty-printer).
- `find` — accept a `:given` filter; when the user types `find <type>`
  and the type is a constraint, surface matching givens prominently.
- `edit` — preserves the given tag on re-save; refusing to drop it
  silently. Re-elaboration follows ADR-016.
- `update` — same as `edit`, plus the ADR-016 failure modes for
  resolution changes.
- `move`, `alias` — copy the given tag to the destination by default.
  Within a namespace, rename preserves givenness; cross-namespace alias
  inherits-by-default per `docs/implicits-plan.md` §5.2.
- `delete` — deleting a given-tagged definition removes both the
  definition and the tag; if other names alias the same hash, the tag on
  those aliases is unaffected (per-name tag, not per-hash).
- `fork` — branch's given-set is forked along with everything else; this
  falls out of `MdValues` already being copied.
- `pull`, `push` — given-set travels with the namespace; depends on the
  Sharing API version bump (ADR-020).

Pros: the noun of the new behavior is "given," which gets its own
verbs; existing commands stay shaped the way users expect. Cons: adds
two commands to the already-long table; documentation and discovery
load.

### Option B: Single `given` command with subcommands

`given list`, `given mark`, `given unmark`. Pros: one entry in the
top-level table. Cons: inconsistent with the existing UCM convention of
flat verbs (`view`, `find`, `update`); breaks tab-completion habits.

### Option C: No new commands; expose marking only through an `edit`-time keyword

Users never run a "mark this given" command; instead, the `given`
keyword in source is the only way to set the tag. Pros: minimal
surface. Cons: defeats the use case of demoting an upstream given
locally without rewriting the source file (a stated design goal in
`docs/implicits-plan.md` §1.1 principle 4); demands a re-edit of code to
toggle a namespace property.

## Decision

We recommend **Option A,** pending ratification: add `givens`,
`mark.given`, `unmark.given`; modify `view`, `find`, `edit`, `update`,
`move`, `alias`, `delete`, `fork`, `pull`, `push` to flow givenness
through their existing semantics. Naming follows the dotted convention
already present in UCM (e.g. `pull.silent`).

## Consequences

- **Unblocks** Phase 3 (UCM integration).
- **Depends on** ADR-013 (knows where the tag lives), ADR-016 (knows
  what `update` does on re-resolution), ADR-018 (commands are
  feature-gated until the flag flips), and ADR-021 (defines what
  `pull`/`push`/`merge` do with conflicting given-sets).
- Edits `InputPatterns.hs` and a corresponding handler in
  `unison-cli/src/Unison/Codebase/Editor/HandleInput/` per command.
  Wires `MdValues` through name-lookup and rendering paths that today
  do not consult metadata (the plan notes `grep -rn MdValues unison-cli`
  returns nothing).
- Required tests: transcript tests for each new command and each
  modified command; round-trip `mark.given` then `unmark.given`
  preserves the term hash; `alias` propagates the tag; `delete` removes
  only the local tag when the hash is multiply-named; help text
  snapshots for every new and modified command.
- LSP commands (Phase 2.F) parallel the UCM surface; this ADR doesn't
  govern them but sets the names and semantics the LSP layer adopts.
