# ADR-019: Implicit-arrow representation in `Type.F`

**Status:** Proposed (foundational, blocking)
**Date:** 2026-05-01
**Deciders:** core maintainers (pending ratification)
**Related:** ADR-001, ADR-004, ADR-014; `docs/implicits-plan.md` §3.3 (gate),
§5.3, §12

## Context

`Type.F` at `unison-core/src/Unison/Type.hs:40` is the base functor
for the type AST:

```
data F a
  = Ref TypeReference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a
```

There is one arrow constructor: ordinary `Arrow a a`. Implicit
parameters introduce a second flavor — the `=>` constructor parsed from
ADR-001 — that the typechecker and elaborator must distinguish from `->`
in order to know which leading parameters become implicit.

This ADR is foundational. ADR-004 (settled) declares "runtime/codegen
untouched"; that is true *only* if the type AST carries the implicit-vs-explicit distinction in a way the elaborator can read at typecheck time
without leaking into runtime forms.

## Options considered

### Option A: Extend `Type.F` with `ImplicitArrow a a`

Add a constructor:

```
| ImplicitArrow a a
```

`Show a => Show (List a)` parses as `ImplicitArrow (App Show a) (App Show
(List a))`. The typechecker, elaborator, and pretty-printer pattern-match
on it; runtime never sees it because `Type` is a compile-time artifact.
Pros: self-describing; every reference site sees implicit-ness directly
in the AST; aligns with how `Effect` is already a peer of `Arrow`; matches
ADR-014's claim that "no new hashing rule" applies at the term level
(the new type-AST constructor is the *only* hashing change needed). Cons:
adds a constructor to a heavily pattern-matched ADT (every consumer must
extend its `case` exhaustively); changes the type-hashing namespace —
every type with `=>` hashes differently from a hypothetical `->` cousin.

The type-hash break is acceptable: `=>` is new syntax; no existing types
contain it; only types created going forward are affected.

### Option B: Side-table on declarations

Keep `Type.F` unchanged; maintain a separate map from `Reference` to
"positions of arrows in this type that are implicit." Pros: preserves
type-hash compatibility for existing types and the *form* of new
implicit-bearing types (the `Type.F` shape is identical to a
non-implicit equivalent). Cons: every consumer of a type must consult the
side-table to know what to do — an effective globalization of the
implicit-or-not signal; ad-hoc inheritance through type substitution
(when a type variable is instantiated, which positions in the resulting
type are implicit?); reverses the locality property that the ABT model
gives us; makes ADR-014's framing weaker (hashing of types becomes
contextually meaningful).

### Option C: Wrapper type `Implicit a`

Introduce a built-in type constructor `Implicit a` such that
`Implicit T -> U` *means* `T => U`. Pros: no `Type.F` change. Cons: the
wrapper leaks into surface syntax (users see `Implicit`); destroys the
visual distinction `=>` was designed for; rejected upstream in
`docs/implicits-plan.md` §5.3.

## Decision

We recommend **Option A: extend `Type.F` with `ImplicitArrow a a`,**
pending ratification. The constructor lives next to `Arrow` and is
treated by the elaborator as "Arrow whose leading parameter is filled by
resolution." The type-hash break is a one-time cost paid only for new
types written with `=>`.

This ADR is **foundational**: it is what makes ADR-004's "runtime/codegen
untouched" tractable, because the elaborator strips `ImplicitArrow` to
ordinary `Arrow` (or never lets it reach runtime in the first place),
and what makes ADR-014's "no new hashing rule" claim coherent.

## Consequences

- **Unblocks** Phase 1 spike (a real or stubbed `Type.F` shape is
  available), Phase 2.C.1, and by transitivity Phase 2.C.2 / 2.D / 2.E.
- **ADRs depending on this one:** 004 (presupposes implicits live in the
  type AST and not in term-level wrappers), 012 (the elaborator pattern-matches on the new constructor), 014 (hashing claim depends on this
  exact shape), 015 (printer renders the constructor as `=>`).
- Every pattern-match on `Type.F` in the codebase must add a case for
  `ImplicitArrow`; the compiler will flag exhaustiveness gaps. Plan a
  sweep across `unison-core`, `parser-typechecker`, `unison-share-api`,
  and the printer modules.
- Type-hash break: any new type containing `=>` hashes differently from
  the `->` form. Document this consequence in the migration guide.
- Required tests: golden-hash tests for representative `=>` types;
  exhaustiveness checks turned on across `Type.F` consumers; a
  unification test confirming that `=>` and `->` do *not* unify (which
  is what makes the elaborator capable of identifying implicit slots).
- Locks in the ABT/`Type.F` extension as the canonical place for
  implicit-vs-explicit. Any future "first-class implicit function
  types" (out of scope per `docs/implicits-plan.md` §1.4) would build
  on this constructor rather than re-litigate it.

## Open question

Whether `ImplicitArrow` should also encode an "implicit-ness arity" for
multi-constraint signatures, or whether `(C1 a, C2 b) =>` desugars to
nested `ImplicitArrow`s. Recommendation: nested form, matching how
`Arrow` already represents curried functions; flag for ratification.
