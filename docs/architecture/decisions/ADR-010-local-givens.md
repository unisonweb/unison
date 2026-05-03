# ADR-010: Local `given` only inside `let`-blocks; no first-class implicit-function types

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-001, ADR-006, ADR-008; `docs/implicits-plan.md` §1.2 ("Local givens"), §1.4 ("not in v1")

## Context

Two related features can be folded into a "local implicit binding"
story. The first is the ability to declare a `given` inside a function
body — e.g. wrapping or transforming an ambient given for the rest of
the block. The second, much heavier, is to have first-class
implicit-function types: `(given x : T) => U`, with anonymous given
lambdas, that can be passed around like any other function. Scala 3
ships both; Idris ships variations.

These features cost very different amounts. Local `given` in a `let`-
block is a small, mechanical addition: it threads a binding through
the lexical scope, where the elaborator already has to track scope.
Anonymous given lambdas require `(given …)` as a binder form, type-
inference for implicit-function types, and changes to how function
values are formed. They also expand the surface area of what
"specificity by lexical proximity" means.

## Options considered

### Option A: Local `given` in `let`-blocks only

Allow `given name : T = expr` as a statement form inside a `let`
block (and equivalent in do-notation). The binding is added to the
lexical given environment for the rest of the block, shadowing any
ambient given of the same type per ADR-008's lexical-inner-wins rule.
No first-class implicit-function types, no anonymous given lambdas.
Pros: small implementation; one new statement form; no changes to
function-type theory; covers every use case in the design plan
(e.g., reversed-order sort by introducing `given local : Ord a =
Ord.flip (summon (Ord a))`); composes with `summon` (ADR-006) and
`@`-override (ADR-007). Cons: users wanting to abstract over
implicit-bearing functions must write the constraint explicitly in
the function type; no `(given x : T) => …` lambda.

### Option B: First-class implicit-function types deferred (current decision but worth naming)

Allow `(given x : T) => U` as a type and `given x => body` as an
expression. Pros: full Scala 3 / Coq parity; `given`-as-binder
generalises uniformly. Cons: requires substantial new type theory
(implicit-function types interact with effect rows, polymorphism,
HKT); requires anonymous-lambda binder syntax; expands what "lexical
inner wins" must reason about; usefulness in real code is small
relative to cost. **Deferred.** Can be added in v2 without breaking
the v1 surface.

## Decision

We adopt **Option A: local `given` only inside `let`-blocks (and
equivalent block forms).** First-class implicit-function types and
anonymous given-lambdas are deferred to a future revision. The local
form is exactly the mechanism shown in `docs/implicits-plan.md`
§1.2:

```
sortReversed : Ord a => [a] -> [a]
sortReversed xs =
  given local : Ord a = Ord.flip (summon (Ord a))
  sort xs
```

## Consequences

- Phase 2.A's parser learns `given <name> : <type> = <expr>` as a
  statement form inside `let` and equivalent block contexts. No new
  expression-position binder form.
- The lexical given environment threaded through
  `Unison.Typechecker.Context` (per `docs/implicits-plan.md` §5.4)
  picks up local givens at the same point it picks up ordinary
  bindings. This is mechanical but pervasive — every binding form
  must extend the env.
- Local givens compose with `summon` (ADR-006): wrapping an ambient
  given is the canonical use case
  (`given local : Ord a = Ord.flip (summon (Ord a))`).
- ADR-008's lexical-inner-wins rule covers shadowing precisely.
  Two locals at the same depth that both match still ambiguate.
- Forecloses (for v1) abstracting over implicit-function-typed
  values. A function that wants "give me a function that consumes a
  `Show`" must take an explicit parameter, not an
  implicit-function-typed argument. This is a real expressiveness
  loss but the workarounds are well-known.
- A future ADR can reintroduce first-class implicit-function types
  on top of v1 without breaking existing code, because every v1
  use of `given` is in binding position; expression-position uses
  are still available namespace.
- Pretty-printer (Phase 4) renders local givens with the same
  syntax users wrote. Round-trip identity is required.
