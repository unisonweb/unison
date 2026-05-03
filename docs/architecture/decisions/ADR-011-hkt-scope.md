# ADR-011: HKT in v1; functional dependencies, associated types, kind-polymorphic givens deferred

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-003, ADR-004, ADR-019; `docs/implicits-plan.md` §1.4 ("What's not in v1"), §12 (kind inference and HKT baseline)

## Context

Typeclass-shaped features sit at different points on a complexity
ladder: rank-1 classes over `Type` (`Eq a`, `Show a`), classes over
type constructors of fixed kind `Type -> Type` (`Functor f`,
`Monad m`), classes that abstract over kind shapes (`Bifunctor`,
classes parameterised by both `Type -> Type` and `Type -> Type ->
Type`), classes with functional dependencies relating parameters
(`Mult a b c | a b -> c`), and classes with associated types (`type
Item c` inside `class Container c`).

Each step adds inference complexity, error-message complexity, and
syntactic surface. Drawing the v1 line decides which patterns
canonical libraries can express now and which patterns block on
future work.

Unison's actual kind system is documented in
`parser-typechecker/src/Unison/KindInference/Generate.hs` line 451:

```haskell
data Kind = Type | Ability | Kind :-> Kind
```

There is no kind polymorphism — `Kind` is closed and finite-shape.
The arrow form supports HKTs of any fixed shape. The transcript
`unison-src/transcripts/idempotent/higher-rank.md` line 52 shows
`unique type Functor f = Functor (forall a b . (a -> b) -> f a -> f
b)` already type-checks, so `f : Type -> Type` works in current
Unison without further kind-system changes.

## Options considered

### Option A: HKT yes; FDs, associated types, and kind-polymorphic givens deferred

Givens may abstract over higher-kinded type constructors of fixed
kind shape (`Functor f` where `f : Type -> Type`, `Monad m`, etc.).
Givens that need functional dependencies, associated types, or
kind variables (e.g., a `Bifunctor`-shaped premise where the kind
shape itself is a parameter) are deferred. Pros: the existing kind
system in `Unison.KindInference` already supports HKT without
extension; the spike (Phase 1) can demonstrate `Functor List` and
`Monad Optional`; the most common typeclass patterns work; no new
kind theory required for v1. Cons: certain classes can't be
expressed cleanly — `MonadReader r m | m -> r` requires FDs;
`Container c` with `Item c` requires associated types — users
write more parameters or duplicate code in the meantime.

### Option B: HKT + FDs + associated types in v1

Push everything into v1. Pros: maximally expressive; matches GHC's
extensions in scope. Cons: each of these is a substantial typechecker
project on its own; dramatically expands the elaborator surface
(associated types interact with type-family normalization);
multiplies error-rendering complexity (FD-violation errors are
notoriously hard to render); pushes v1 ship date out by months;
none are blocking for the headline use cases (`Eq`, `Ord`,
`Functor`, `Monad`, `Traversable`).

### Option C: Rank-1 only; no HKT

Restrict v1 to classes over `Type`. Pros: simplest possible v1.
Cons: rules out `Functor`, `Monad`, `Traversable`, the entire stdlib
seed plan in Phase 5; cripples the feature for typed-functional
idioms; would require a v2 to be useful for serious work.

## Decision

We adopt **Option A: HKT in v1; functional dependencies, associated
types, and kind-polymorphic givens deferred to v2.** The kind system
needs no extension to support v1 — the existing `Type | Ability |
Kind :-> Kind` definition handles every v1 case.

## Consequences

- Phase 5's stdlib seed (`Eq`, `Ord`, `Show`, `Functor`,
  `Applicative`, `Monad`, `Traversable`) can be authored in v1
  without further language work. The HKT cases (`Functor`, `Monad`,
  `Traversable`) reuse the kind machinery already in
  `parser-typechecker/src/Unison/KindInference/`.
- The spike (Phase 1) explicitly tests `Functor List` and `Monad
  Optional` to confirm HKT given resolution works.
  `unison-src/transcripts/idempotent/higher-rank.md` line 52 is the
  baseline showing HKT already works at the language level.
- Functional-dependency-shaped abstractions (state monads with
  recoverable state type, `IsString`-style coercions) cannot be
  expressed as cleanly in v1. Users either parameterise over the
  related types explicitly or wait for v2.
- Associated types similarly deferred. The `Ord` example in
  `docs/implicits-plan.md` §1.2 sidesteps this by storing an `eq :
  Eq a` *value* inside the record rather than declaring an
  associated `type Item`.
- Kind-polymorphic givens (e.g., a `Functor` instance parameterised
  by the kind of the wrapped functor) are explicitly out. Standard
  HKT — fixed kind shape — works.
- Locks the v1 elaborator into "kind shape is determined by the
  given's type signature; no kind variables are introduced during
  resolution." Any future work that adds kind polymorphism must
  revisit this assumption.
- Documentation (Phase 6 migration guide) lists FDs and assoc types
  as known omissions with examples of how to work around them in
  v1.
