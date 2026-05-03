# ADR-018: Feature-flag rollout

**Status:** Proposed
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-017, ADR-022; `docs/implicits-plan.md` §9

## Context

Implicit parameters are a major addition to a content-addressed,
production-relied-on language. Even with a careful staged rollout
(Phases 0–6), there are knowable unknowns: surprise interactions with
TDNR, edge cases the spike did not exercise, ergonomics issues that
only show up under real workloads, and cross-library effects in shared
code. We want a way to make the feature available for experimentation
*without* committing every user to its presence in the language.

The design space is which dimension the flag operates on: per-project
(opt-in via project config), per-binary (a compile-time switch baked
into the UCM build), or just-ship-it (no flag at all). The answer
shapes how Phase 6 rolls out and how the keyword migration in ADR-022
interacts with code that does not opt in.

## Options considered

### Option A: Per-project feature flag in project config

A project's config records whether implicits are enabled. UCM, the
typechecker, the parser, and the printer all read the flag. With the
flag off: `=>`, `given`, `summon`, and `@` in expression position
remain unrecognized; `given` and `summon` continue to be legal
identifiers (per ADR-022). With the flag on: implicits are fully
available. Pros: gradual ecosystem rollout; library authors opt in when
ready; users who depend on `given`/`summon` as identifiers are unaffected
until they choose to upgrade; the flag flips to "on by default" in a
later release without a binary change. Cons: requires a project-config
schema bump and per-project state to thread through the typechecker;
two code paths to keep working through Phase 5.

### Option B: Compile-time flag in the UCM binary

The flag is a Cabal/build setting. Production builds ship without the
feature; nightly/dev builds ship with it. Pros: simplest to implement;
no config plumbing. Cons: forces every user of a build to be either
fully in or fully out; downstream packagers must choose; bug reports
fragment by build flavor; mixing givens-using and givens-not-using
projects in the same UCM session becomes impossible.

### Option C: Immediate on, no flag

Ship the feature enabled in the next release. Pros: simplest user
story; no flag plumbing. Cons: leaves no path to recover from
ecosystem-level surprises; conflicts with ADR-022's keyword migration
(any code using `given` or `summon` as identifiers breaks day one); no
soft landing if the resolver has performance regressions on large
codebases.

## Decision

We recommend **Option A: a per-project feature flag in project
config,** pending ratification. The flag defaults to *off* through one
release cycle for library authors to experiment, then flips to *on by
default* in the following release. Library authors who need the feature
opt in; library consumers see no change until their dependencies start
using it.

## Consequences

- **Unblocks** Phase 6 (release) — the rollout sequence has a clear
  mechanism.
- **Depends on** ADR-017 (the new commands respect the flag) and ADR-022
  (the keyword reservation is gated on the flag for one release cycle,
  so a project not opting in keeps the legacy meaning of `given` and
  `summon`).
- The typechecker, parser, and printer all need a single-bit "implicits
  on" channel; plumbing this through is a Phase 2 task. Avoid the
  temptation to spread the flag check through many sites — concentrate
  it where parser modes branch (`unison-syntax`) and where elaboration
  begins (Typechecker / FileParsers).
- Required tests: a project with the flag off rejects `=>` syntax; the
  same project with the flag on accepts it; a project with the flag off
  still parses `given` and `summon` as identifiers; flipping the flag
  on a project that uses those identifiers as definitions raises a
  migration error per ADR-022; CI matrix runs both flag states until
  the default flips.
- Adds a permanent obligation: even after the default flips on,
  documentation must call out how to opt out (e.g. for a library
  pinned to legacy syntax for one more release). Removing the off-state
  entirely is a future ADR.
