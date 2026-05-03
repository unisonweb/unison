# ADR-003: A "class" is an ordinary type; no new declaration kind

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-002, ADR-004, ADR-011; `docs/implicits-plan.md` §1.2 ("Declaring a class")

## Context

Languages with implicit/typeclass features split into two camps. One
camp (Haskell, Idris, PureScript, Rust) introduces a dedicated `class`
or `trait` declaration form whose bodies define methods, with `instance`
declarations as the dual. The other camp (Scala 3, Coq) treats a "class"
as just a type — usually a record — and instances as ordinary values of
that type.

Unison already has rich record-via-data-constructor syntax. The
`unique type` form supports parameterised constructors with named
fields, and pattern-matching on those constructors gives you method
extraction for free. The `higher-rank.md` transcript
(`unison-src/transcripts/idempotent/higher-rank.md` line 52) shows
`unique type Functor f = Functor (forall a b . (a -> b) -> f a -> f b)`
already type-checks today. Adding a separate `class`/`instance` system
would either duplicate or clash with these mechanisms.

## Options considered

### Option A: Classes are ordinary types

A "class" is a type declaration, typically a record:
`unique type Show a = Show { show : a -> Text }`. An "instance" is just
a value of that type, optionally tagged `given`. Pros: no new
declaration kind to design, parse, hash, or store; subclassing falls
out of records-containing-records (`Ord` carries a `functor : Functor
m` field); existing typechecker, codegen, hashing, and UCM commands
work unmodified; aligns with how community libraries already encode
typeclass-shaped abstractions today. Cons: no `class`-specific syntax
sugar (e.g., automatic method-projection by type); subclass coercion
is manual record access rather than implicit.

### Option B: Dedicated `class` and `instance` declarations (Haskell-style)

Add new top-level forms: `class C a where method : Sig` and
`instance C T where method = …`. Pros: methods can be projected by name
without an explicit dictionary handle; superclasses can be
implicitly-inserted upcasts; closer match to Haskell idioms. Cons:
introduces a new declaration kind that has to flow through every
codebase concern — parser, AST, typechecker, hashing, namespace
storage, UCM `view`/`find`/`edit`, sharing protocol, pretty-printer,
codebase migration. Doubles the surface area of the feature for a
gain that records-with-`given`-values mostly already provide. Rules
out a downstream user reclassifying an upstream record as a "class"
without re-declaring it.

## Decision

We adopt **Option A: classes are ordinary types**, typically records.
There is no dedicated `class` or `instance` declaration form. A given
is simply a value of a record type with the `given` namespace tag.

## Consequences

- The whole feature ships without a new declaration kind. Parser, AST,
  typechecker, hashing, codebase storage, and UCM commands carry over
  unchanged for class/instance machinery — only the implicit-resolution
  pass and the namespace `given` tag are new.
- Method invocation goes through ordinary record field access, e.g.
  `Show.show (summon (Show a)) x`. With implicit resolution this
  collapses to the ergonomic form once the elaborator inserts the
  dictionary, but the underlying mechanism is field projection.
- Subclassing is composition of records. `Ord` containing a `functor :
  Functor m` field is the entire mechanism — there is no "superclass
  upcast" rule, the user calls the field. This is simpler but more
  manual than Haskell's superclass inference.
- Any existing `unique type` already in the wild can be retroactively
  used as a class; existing record values can be retroactively tagged
  `given`. There is no "class-ification" migration step.
- Locks us out of class-only syntactic sugar (associated types,
  defaults, automatic superclass coercion). ADR-011 already defers
  associated types and functional dependencies; these would have been
  natural to attach to a `class` form but must be revisited if added.
- Keeps the door open for adding a `class` *abbreviation* later as
  pure sugar over `unique type` + `given`, without re-architecting
  anything.
