# Type Aliases in Unison

This doc describes the design for type aliases in Unison: surface syntax, semantics,
elaboration, codebase storage, pretty-printing, and update propagation.

## Goal

Let users introduce names for type and ability-row expressions for ergonomics and
documentation, without those names participating in type identity. A type alias is
a *display + parse-time* concept; it is never part of the hashed form of any
definition that uses it.

## Surface syntax

```
type alias Endo a       = a -> a
type alias Pair a b     = (a, b)
type alias StringMap v  = Map Text v
type alias Web          = {IO, Exception, Http}
```

Parameters follow the alias name, mirroring `type` declarations. There is no
separate `ability alias` keyword: kind inference determines from the RHS whether
the alias is a type alias (kind `* -> ... -> *`) or an ability-row alias.

Aliases must be **fully saturated** at every use site. Partial application of an
alias is rejected by the elaborator (Unison has no type-level lambdas to represent
the partial result).

Recursive aliases are forbidden — `type alias A = B` paired with `type alias B = A`
is rejected by the kind checker. Recursion stays in `type` declarations.

## Semantics: transparent

An alias is a name bound to a parameterized type expression. At every use site,
the alias is fully expanded to its RHS before the surrounding term or declaration
is hashed.

Consequences:

- `f : Username -> Text` and `f : Text -> Text` (with `type alias Username = Text`)
  produce the same term hash, bit-for-bit.
- The typechecker treats `Username` and `Text` as definitionally equal — there is
  nothing to unify.
- Aliases never appear in any hashed form.

## Elaboration

For a **saturated type alias** `Alias a₁ ... aₙ = Body`:

The elaborator substitutes the argument types for `a₁, ..., aₙ` in `Body` and
splices the result into the surrounding type tree.

For an **ability-row alias** `Web = {e₁, ..., eₖ}`:

When `Web` appears as an element of an `Effects [...]` list during elaboration,
the elaborator replaces that element with `[e₁, ..., eₖ]`. The surrounding list
is then deduped — ability rows have **Set semantics**, regardless of how `F.Effects`
stores them as `[a]`.

Misuse — an ability-row alias appearing in a type position, or a type alias
appearing as an element of an ability row — produces an ill-kinded type that the
kind checker rejects with its usual error machinery. No bespoke error story.

## The no-leakage invariant

> No `Reference` tagged `RtTypeAlias` may appear inside a `Type v a` that is about
> to be hashed, persisted, or sent to the runtime.

This is enforced by **discipline + checks** at three boundaries:

1. `Hashing.V2.Type.hashType` runs a structural walk and refuses to hash a
   `Type v a` containing any alias ref.
2. The codebase write path (`putTerm`, `putTypeDeclaration`, etc.) runs the same
   check before persisting a body.
3. End-of-elaboration: the same check, signaling an elaborator bug if it fails.

A leakage check is a single-pass walk; non-hot path; cheap.

```haskell
checkNoAliasRefs ::
  (TypeReference -> ReferenceType) -> Type v a -> Either TypeReference ()
checkNoAliasRefs refType =
  ABT.foreachSubterm $ \case
    Ref' r | refType r == RtTypeAlias -> Left r
    _ -> Right ()
```

## References and the codebase

### `ReferenceType`

```haskell
data ReferenceType = RtTerm | RtType | RtTypeAlias
```

A single new tag `RtTypeAlias`, shared by both type aliases and ability-row
aliases. The structural `Reference'` is unchanged — aliases use ordinary
`Derived(hash, 0)` references. The discriminator lives at the `ReferenceType`
layer where it already belongs.

Why one tag, not two:
- Both kinds of alias are "name → type expression" entries; the difference is the
  RHS's kind, not the entry kind.
- The entry itself stores its RHS, so the kind is recoverable on lookup.

Why not a new `Reference'` constructor:
- Bigger blast radius (every pattern match on `Reference'` would need updating).
- Hash structure is identical.
- The natural discriminator is "what does this point to," which is what
  `ReferenceType` exists for.

### Namespace / `Branch`

Aliases share the `_types` slot in `Branch0` with data declarations. Name
collisions span both kinds: you can't have a data decl `Foo` and an alias `Foo`
in the same namespace.

`Names` is unchanged:

```haskell
data Names = Names
  { terms :: Relation Name Referent
  , types :: Relation Name TypeReference   -- now covers aliases too
  }
```

`find` and `view` work without dispatch — existing type-name lookup hits aliases
for free.

### On-disk storage

A new SQLite table parallel to the decl tables:

```sql
CREATE TABLE type_alias (
  id          INTEGER PRIMARY KEY,
  hash        BLOB NOT NULL,
  pos         INTEGER NOT NULL,    -- always 0; aliases are not in components
  param_count INTEGER NOT NULL,    -- denormalized arity for cheap lookup
  body_blob   BLOB NOT NULL        -- serialized parameterized Type ABT
);
```

Aliases are always single-element "components" (`pos = 0`) because they cannot
be recursive — there are no cycles to package.

### Codebase API additions

```haskell
data TypeAlias = TypeAlias
  { paramNames :: [v]              -- preserved for display
  , body       :: Type v ()        -- bound by paramNames
  }

data TypeEntry
  = TypeDecl   (DataDeclaration v ())
  | TypeAliasE TypeAlias

getTypeEntry  :: TypeReferenceId -> m (Maybe TypeEntry)
putTypeAlias  :: TypeReferenceId -> TypeAlias -> m ()
isTypeAlias   :: TypeReference  -> m Bool
aliasArity    :: TypeReference  -> m (Maybe Int)
```

Existing `getTypeDeclaration` keeps its signature; calls on an alias ref return
`Nothing`. New code routes through `getTypeEntry`.

## Pretty-printing via e-graph rewriting

When rendering a type expression, the PPE folds the expression back into in-scope
aliases. This is implemented as an e-graph rewrite step over a display-only
mirror of `F`, using `hegg` (the Haskell port of egg).

### Display-side node functor

```haskell
data PF v a
  = PVar        v
  | PRef        TypeReference
  | PArrow      a a
  | PApp        a a
  | PEffect     a a
  | PEffects    [a]                 -- canonicalized: sorted, deduped
  | PForall     v a
  | PIntroOuter v a
  | PAnn        a K.Kind
```

`PF` is a structural mirror of `F` with two adjustments: ABT binders are
flattened (`PForall v a`, `PVar v`), and variables are preserved at type `v` so
the e-graph does capture-free matching naturally (two distinct binders with the
same display name produce distinct `PVar` nodes; an alpha-equivalent type matches
the same `PArrow ?x ?x` pattern because both arms reference the same e-class).

There is **no `PAlias` node**. Alias applications are structurally just
`PApp (PRef aliasRef) args` — identical to any other type application. The fact
that a ref happens to point to an alias entry is a property of the namespace
environment, not of the tree shape.

### Conversion

`toPF` and `fromPF` are environment-free and mechanical:

```haskell
toPF   :: Type v a -> Fix (PF v)
fromPF :: (Ord v, Monoid a) => Fix (PF v) -> Type v a
```

Round-trip is identity for any type no rewrite rule fires on. Aliases are folded
in (or expanded out) entirely by the e-graph step.

### Rewrite rules

Each alias in scope contributes one rewrite rule from RHS to alias form.

```
R1 (Endo a = a -> a):
  (PArrow ?a ?a) ↦ (PApp (PRef #EndoRef) ?a)

R2 (StringMap v = Map Text v):
  (PApp (PApp (PRef #MapRef) (PRef #TextRef)) ?v)
    ↦ (PApp (PRef #StringMapRef) ?v)

R3 (Web = {IO, Exception, Http}):
  (PEffects ⟨#ExceptionRef, #HttpRef, #IORef | ?rest⟩)
    ↦ (PEffects ⟨#WebRef | ?rest⟩)
```

R3 is a **partial row fold**: it matches a row containing the alias's RHS
elements as a subset, binding the remainder to `?rest`. This requires a small
extension to the pattern matcher (row-pattern primitive: check subset against the
row's canonical list, bind the difference). Subset check is cheap because rows
are sorted at insertion.

### Cost function

```haskell
cost :: PF v Cost -> Cost
cost = \case
  PVar _   -> 1
  PRef r   -> case refType r of
    RtTypeAlias | inLocalScope r  -> 0
                | inAmbientScope r -> 1
                | otherwise        -> 1
    _ -> 1
  PApp a b -> 1 + a + b
  PArrow i o -> 1 + i + o
  PEffect e t -> 1 + e + t
  PEffects rs -> 1 + sum rs
  PForall _ b -> 1 + b
  PIntroOuter _ b -> 1 + b
  PAnn b _ -> 1 + b
```

The 0-cost for in-scope aliases is what implements "longest-match wins" without
explicit sequencing: collapsing more nodes always lowers cost.

Tie-breaking: lexicographic on the alias's hash-qualified name. Deterministic,
independent of rule insertion order.

### Pipeline

```
prettyType :: PPE -> AliasEnv -> Type v a -> Doc
prettyType ppe env t =
  existingPrettyPrinter ppe
    . fromPF
    . extract (costFn env)
    . saturate (rulesFor env)
    . toPF
    $ t
```

The existing pretty-printer handles the post-rewrite `Type v a` as-is — it
already renders `App (Ref EndoRef) Nat` as "Endo Nat" because the PPE names
refs. The alias-aware pass is purely a structural rewrite that happens before
rendering.

### Saturation budget

Tight iteration cap (e.g., 5 rounds) and a node-count limit. Types are small;
pretty-printing must not block. Cache extraction results keyed on
`(type, alias-set-hash)`.

## Update propagation

Aliases are **inbound participants** and **outbound silent**.

- When something in an alias's RHS changes hash, the alias body blob changes, the
  alias hash changes, and the alias's name is rebound to the new hash. Automatic
  via the existing update machinery, because aliases register dependency edges
  out (alias → things in its RHS) the same way decls do.
- Nothing's hash depends on an alias's hash structurally (terms were hashed
  against the expansion, not the alias). So `dependents` of an alias is empty by
  construction, and alias updates ripple no further.

Consequences:

- An alias always reflects the *current* version of its dependencies. Drift is
  impossible.
- Deleting an alias is harmless to existing definitions — their hashes are
  already against the expansion.
- `dependents Endo` returns nothing. Users wanting to know "where is this alias
  mentioned in source?" need a separate *source* query (re-render term types
  through the PPE folding pass and check which ones fold to mention the alias).

## What changes elsewhere

- **Parser**: accept `type alias <name> <params> = <type-expression>` at the
  top level.
- **Elaborator**: substitution for type alias use sites; splicing + dedup for
  ability-row alias use sites; saturation check.
- **Kind checker**: reject recursive aliases; reject misuse (type alias in
  ability position, ability alias in type position) as ordinary kind errors.
- **Hashing**: run the no-leakage check before producing a hash.
- **Codebase**: new `type_alias` table; `getTypeEntry`, `putTypeAlias`,
  `isTypeAlias`, `aliasArity`.
- **Branch / Names**: no structural change; aliases live in the `_types` slot.
- **PPE**: new e-graph rewrite stage between `Type v a` ingress and rendering.
- **Wire format (sync)**: add the alias entry kind, mirroring how decls are
  serialized.
- **UCM**: `find`, `view`, `dependents`, `rename`, `delete` extend over aliases
  in the obvious way — `find` and `view` work transparently since aliases share
  the types slot; `rename` and `delete` are routine codebase operations with no
  blast radius.

## Non-goals

- Alias-as-refactoring-leverage. Because alias use sites are hashed against the
  expansion, changing an alias's source does not retroactively update existing
  definitions. This is a feature of the transparent design, not a bug. Users
  wanting refactoring leverage should use `update` on the underlying type.
- Opaque aliases (Scala-3-style). Reserved for a future `opaque type` feature,
  separate from this design.
- First-class type-level lambdas. Partial application of an alias is rejected.
