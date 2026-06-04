# TypeTag Implementation Plan

## Session Info

- **Branch:** `baccata/typetags`
- **Base commit:** `8b6462c` (Record passing proofs)
- **Status:** COMPLETE — all steps implemented

---

## Overview

Adding a built-in `TypeTag` type of kind `* -> *` that carries a structured, serializable, introspectable representation of a fully-applied type at runtime. The typechecker auto-synthesizes `TypeTag` values via the implicit (`=>`) elaboration system.

### Design Decisions

- **Structured representation** (not just a hash): `TypeTagRepr` is a recursive ADT mirroring the type AST
- **Monomorphic synthesis**: ground types get a compile-time literal
- **Polymorphic threading**: `TypeTag a =>` flows through the normal given-resolution mechanism
- **Compiler-produced literal**: users cannot construct `TypeTagLit` manually — only the elaborator can
- **Foreign-wrapped at runtime**: `WrapTypeTag TypeTagRepr` in the `Foreign` union

---

## Commits

| Commit | Description |
|--------|-------------|
| `4d424a1` | Type system layer (TypeTagRepr ADT, builtin type, term constructor, elaboration) |
| `7966ea2` | Runtime layer (ANF, MCode, Machine, Foreign, serialization) |
| `e0b049a` | Polymorphic TypeTag threading via normal resolution |
| `fbfcc74` | `TypeTag.toText` and `TypeTag.references` builtins |
| `d795abd` | Transcript test |
| `1fbe5b0` | V2 codebase format for SQLite persistence |
| `6d70b1c` | Fix TypeTagLit hashing round-trip (HashTypeTagRepr) |
| `56c22e9` | `TypeTag.serialize` and `TypeTag.deserialize` builtins |
| `d932832` | Fix deserialize Optional tag construction |

---

## Architecture

### TypeTagRepr ADT

**File:** `unison-core/src/Unison/TypeTagRepr.hs`

```haskell
data TypeTagRepr
  = TTRef !TypeReference
  | TTApp !TypeTagRepr !TypeTagRepr
  | TTArrow !TypeTagRepr !TypeTagRepr
  | TTEffect ![TypeTagRepr] !TypeTagRepr
```

No `Forall` or `Var` — only ground types are representable.

Exports: `typeToRepr`, `typeTagRefs`, `updateTypeTagRepr`

### Builtin Type Registration

- `unison-core/src/Unison/Type.hs`: `typeTagRef = Reference.Builtin "TypeTag"`, `typeTag` smart constructor
- `parser-typechecker/src/Unison/Builtin.hs`: `B' "TypeTag" CT.Data` in `builtinTypesSrc`
- `parser-typechecker/src/Unison/KindInference/Generate.hs`: registered with kind `Type :-> Type`

### Term AST

**File:** `unison-core/src/Unison/Term.hs`

- `TypeTagLit TypeTagRepr` constructor in the `F` data type
- Pattern `TypeTagLit'`, smart constructor `typeTagLit`
- Handled in all traversals: `Eq`, `Show`, `extraMap`, `generalizedDependencies`, `updateDependencies`

### Elaboration (how TypeTag values are synthesized)

**`parser-typechecker/src/Unison/Typechecker/GivenElaborator.hs`:**

`resolveOne` intercepts before normal resolution:
- `matchTypeTagGoal`: detects `App (Ref typeTagRef) inner`
- If `inner` has no free vars → produce synthetic `ResolutionTree` with sentinel `Reference.Builtin "TypeTag.synth"`
- Otherwise → fall through to normal given-resolution (polymorphic threading)

**`parser-typechecker/src/Unison/Typechecker/GivenApply.hs`:**

`buildDictionary` detects the `"TypeTag.synth"` sentinel:
- Calls `TypeTagRepr.typeToRepr` on the `givenConclusion`
- Produces `Term.typeTagLit ann repr`

### Runtime Pipeline

| Layer | File | What |
|-------|------|------|
| ANF | `unison-runtime/src/Unison/Runtime/ANF.hs` | `LTT TypeTagRepr` literal, classified as BX (boxed) |
| MCode | `unison-runtime/src/Unison/Runtime/MCode.hs` | `MTT TypeTagRepr` machine literal |
| Foreign | `unison-runtime/src/Unison/Runtime/Stack.hs` | `WrapTypeTag TypeTagRepr` + `BuiltinForeign` instance |
| Machine | `unison-runtime/src/Unison/Runtime/Machine.hs` | `MTT repr → BoxedVal (Foreign (WrapTypeTag repr))` |
| Decompile | `unison-runtime/src/Unison/Runtime/Decompile.hs` | `WrapTypeTag repr → typeTagLit () repr` |

### Serialization

- **ANF format:** `unison-runtime/src/Unison/Runtime/ANF/Serialize.hs` and `CodeV4.hs` — tag `LTTT` (7 in `LtTag`)
- **MCode format:** `unison-runtime/src/Unison/Runtime/MCode/Serialize.hs` — tag `MTTT` (7 in `MLitT`)
- **Prim1 tags:** `unison-runtime/src/Unison/Runtime/Serialize.hs` — `TAGT` = 66, `TAGR` = 67, `TAGS` = 68, `TAGD` = 69
- **SQLite/V2:** `codebase2/codebase-sqlite/.../Serialization.hs` — tag 22, recursive `putTypeTagReprV2`/`getTypeTagReprV2`

### Builtin Operations

| Builtin | Type | Primitive |
|---------|------|-----------|
| `TypeTag.toText` | `forall a. TypeTag a -> Text` | `TAGT` |
| `TypeTag.references` | `forall a. TypeTag a -> List Link.Type` | `TAGR` |
| `TypeTag.serialize` | `forall a. TypeTag a -> Bytes` | `TAGS` |
| `TypeTag.deserialize` | `forall a. Bytes -> Optional (TypeTag a)` | `TAGD` |

Equality is handled by `Universal.==` (structural `Eq` on the `Foreign` value).

**Serialization format** (implemented in `Machine/Primops.hs`): recursive varint-encoded binary. Tag bytes: 0=Ref, 1=App, 2=Arrow, 3=Effect. References encoded as 0+text (builtin) or 1+hash+pos (derived). Text/hash lengths use varint encoding.

**Note on Optional construction**: runtime Optional values must use `Ty.noneTag`/`Ty.someTag` (from `Runtime.TypeTags`), NOT raw `PackedTag 0`/`PackedTag 1`. The packed tags incorporate reference-level information the pattern matcher requires.

### V2 Codebase Format

**`codebase2/codebase/U/Codebase/Term.hs`:**

```haskell
data TypeTagReprV2 typeRef
  = TTRef typeRef
  | TTApp (TypeTagReprV2 typeRef) (TypeTagReprV2 typeRef)
  | TTArrow (TypeTagReprV2 typeRef) (TypeTagReprV2 typeRef)
  | TTEffect [TypeTagReprV2 typeRef] (TypeTagReprV2 typeRef)
```

The V2 repr is parameterized over `typeRef` so it integrates with the V2 reference scheme. Conversion functions `typeTagRepr1to2`/`typeTagRepr2to1` in `Conversions.hs`.

---

## How It Works End-to-End

1. User writes: `show : TypeTag a => a -> Text`
2. At a call site like `show 42`, the typechecker instantiates `a := Nat` and emits a `ConstraintGoal` for `TypeTag Nat`
3. `GivenElaborator.resolveOne` detects `TypeTag Nat` via `matchTypeTagGoal`
4. Since `Nat` has no free vars, produces synthetic `ResolutionTree` with sentinel ref `"TypeTag.synth"` and stores `Nat` in `givenConclusion`
5. `GivenApply.buildDictionary` sees the sentinel, calls `typeToRepr` on the conclusion, produces `TypeTagLit (TTRef natRef)`
6. ANF converts to `TLit (LTT (TTRef natRef))`
7. MCode emits `Lit (MTT (TTRef natRef))`
8. Machine evaluates to `BoxedVal (Foreign (WrapTypeTag (TTRef natRef)))`
9. If `TypeTag.toText` is called, primitive `TAGT` extracts the `TypeTagRepr` and `show`s it

For polymorphic threading (`TypeTag a => ...` calling another `TypeTag a => ...`), step 3 falls through to normal resolution which finds the enclosing given.

---

## Possible Future Extensions

- **Level 2 composition**: building `TypeTag (List a)` from `TypeTag a` at runtime. Requires runtime constructor builtins (`TypeTag.ref : Link.Type -> TypeTag Any`, `TypeTag.app : TypeTag f -> TypeTag a -> TypeTag (f a)`, etc.) so the elaborator can synthesize code that combines static structure with a dynamic `TypeTag a` value at runtime.
- **Pattern matching on structure**: expose a `TypeTag.match` builtin or a conversion to a Unison-visible ADT (e.g. `TypeTag.toStructure : TypeTag a -> TypeRepr` where `TypeRepr` is a Unison data type with `Ref`, `App`, `Arrow`, `Effect` constructors). This would let user code dispatch on the shape of a type at runtime.
- **Better `toText`**: currently uses Haskell's `show` on the repr (outputs `TTRef (ReferenceBuiltin "Nat")`). Could pretty-print with resolved names (e.g. `"Nat"`, `"List Nat"`) by consulting the codebase's name lookup.
- **Equality with type-level guarantee**: currently `TypeTag.==` uses `Universal.==` which has type `a -> a -> Boolean` — it works but returns `Boolean`, not a type-level proof. A future `TypeTag.sameType : TypeTag a -> TypeTag b -> Optional (Proof (a == b))` could provide type-safe casting.
- **Hashing**: `TypeTag.hash : TypeTag a -> Hash` — compute the content hash of the type structure directly, for use as map keys or deduplication without serializing to Bytes first.
