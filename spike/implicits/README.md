# `unison-implicits-spike`

Phase 1 spike for Unison's implicit-parameters feature. **This is throwaway
code**: a standalone Haskell package whose only purpose is to validate the
resolution algorithm described in
[`docs/implicits-plan.md`](../../docs/implicits-plan.md), sections 1.3 and 4,
before any production code is touched.

It is intentionally isolated from the main build:

- Not listed in the top-level `stack.yaml`.
- Has its own `cabal.project` (pinned to GHC 9.10.3 to match `lts-24.21`,
  but compiles on 9.6+).
- Depends only on `base`, `containers`, `tasty`, `tasty-hunit`, `time`. It does
  *not* depend on any other Unison package.

## Building and testing

```sh
cd spike/implicits
cabal test
```

If your system has a different GHC, edit `cabal.project` (`with-compiler:`)
or remove that line entirely.

## What it validates

All six success criteria from §4.2 of the plan:

1. **Cycles terminate.** Per-branch cycle detection via a stack of in-flight
   goals.
2. **Specificity ordering.** Subsumption (more-specific wins) plus
   lexical-inner-wins for local givens.
3. **HKT.** Higher-kinded givens resolve when the type representation is
   curried application (`TApp`).
4. **Ambiguity.** Two unrelated givens of the same type produce an
   `Ambiguous` error listing both.
5. **Performance.** A pool of ~1000 givens with chains ~20 deep resolves in
   well under a second on commodity hardware.
6. **Diamond dependencies.** Sub-results are memoized within a single
   top-level resolution; the same goal type is solved exactly once even
   when reached along multiple chain paths. Verified via an instrumented
   counter exposed by `resolveCounted`.

## Layout

```
spike/implicits/
  unison-implicits-spike.cabal
  cabal.project
  README.md
  src/
    Implicits/Types.hs   -- Ty, Given, Pool, ResolutionTree, errors
    Implicits/Unify.hs   -- Robinson's algorithm over Ty
    Implicits/Resolve.hs -- the resolution algorithm proper
  test/
    Main.hs              -- tasty + tasty-hunit, six success criteria
```

The Phase 1 gate writeup ("what worked / surprises / plan refinements")
is delivered separately alongside this spike rather than committed as
a file here. See the parent task's response.

## Non-goals

- Not tied to Unison's real `Type.F`. The mock `Ty` is plain enough for
  unification + HKT without smuggling in ABT, kinds, abilities, etc.
- No parser. Pools are hand-built in tests.
- No namespace concept. The `Scope` field on a given is `Lexical Int |
  Ambient`; nothing else.
- No TDNR integration. None of the production code is touched.
