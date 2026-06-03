# TypeTag Implementation Plan

## Session Info

- **Session ID:** `03071c4c-dad8-44a3-ade4-3fa54ed329f4`
- **Branch:** `prototype/implicits-rfc`
- **Base commit:** `8b6462c` (Record passing proofs)
- **Date:** 2026-06-03

---

## Overview

Adding a built-in `TypeTag` type of kind `* -> *` that carries a structured, serializable, introspectable representation of a fully-applied (monomorphic) type at runtime. The typechecker auto-synthesizes `TypeTag` values via the implicit (`=>`) elaboration system.

### Design Decisions

- **Structured representation** (not just a hash): `TypeTagRepr` is a recursive ADT mirroring the type AST
- **Monomorphic only**: free type variables in the goal cause a resolution error
- **Compiler-produced literal**: users cannot construct `TypeTagLit` manually — only the elaborator can
- **Foreign-wrapped at runtime**: `WrapTypeTag TypeTagRepr` in the `Foreign` union

---

## Steps

### Step 1: Define `TypeTagRepr` ADT -- DONE

**New file:** `unison-core/src/Unison/TypeTagRepr.hs`

```haskell
data TypeTagRepr
  = TTRef !TypeReference
  | TTApp !TypeTagRepr !TypeTagRepr
  | TTArrow !TypeTagRepr !TypeTagRepr
  | TTEffect ![TypeTagRepr] !TypeTagRepr
```

Also exports: `typeToRepr`, `typeTagRefs`, `updateTypeTagRepr`

### Step 2: Register `TypeTag` as a Builtin Type -- DONE

- `unison-core/src/Unison/Type.hs`: added `typeTagRef = Reference.Builtin "TypeTag"` and `typeTag` smart constructor
- `parser-typechecker/src/Unison/Builtin.hs`: added `B' "TypeTag" CT.Data` to `builtinTypesSrc`
- `parser-typechecker/src/Unison/KindInference/Generate.hs`: registered with kind `Type :-> Type`

### Step 3: Add `TypeTagLit` to Term AST -- DONE

- `unison-core/src/Unison/Term.hs`:
  - Added `TypeTagLit TypeTagRepr` constructor to `F`
  - Added pattern `TypeTagLit'`
  - Added smart constructor `typeTagLit`
  - Extended `Eq`, `Show`, `extraMap`, `generalizedDependencies`, `updateDependencies`

### Step 4: Special-Case Elaboration -- DONE

- `parser-typechecker/src/Unison/Typechecker/GivenElaborator.hs`:
  - `matchTypeTagGoal`: detects `App (Ref typeTagRef) inner`
  - `resolveOne`: intercepts before normal resolution; checks monomorphicity via `Set.null (ABT.freeVars innerType)`
  - `syntheticTypeTagTree`: produces a synthetic `ResolutionTree` with sentinel `Reference.Builtin "TypeTag.synth"`
  - Exports `typeTagSynthRef` for detection in GivenApply

- `parser-typechecker/src/Unison/Typechecker/GivenApply.hs`:
  - `buildDictionary`: detects sentinel ref, calls `TypeTagRepr.typeToRepr` on the `givenConclusion` to produce a `TypeTagLit` term

### Step 5: Typechecker Synthesis -- DONE

- `parser-typechecker/src/Unison/Typechecker/Context.hs`:
  - Added `| Term.TypeTagLit' _ <- e = pure (Type.app l (Type.typeTag l) (Type.any l), [])` in `synthesizeWanted`
  - (Needed for round-tripping; during normal compilation, `TypeTagLit` is inserted *after* typechecking)

### Step 6: Hashing / Codebase Storage (Partial) -- DONE

- `unison-hashing-v2/src/Unison/Hashing/V2/Term.hs`: added `TermTypeTagLit [Reference]` with tag 20
- `parser-typechecker/src/Unison/Hashing/V2/Convert.hs`: `m2hTerm` maps `TypeTagLit repr` to `TermTypeTagLit (map m2hReference (typeTagRefs repr))`; reverse conversion errors (not expected to be needed)
- `parser-typechecker/src/Unison/Codebase/SqliteCodebase/Conversions.hs`: V1→V2 errors with message (V2 format doesn't have the constructor yet)

### Step 7: Exhaustiveness Fixes -- DONE

Pattern matches extended in:
- `unison-merge/src/Unison/Merge/Synhash.hs`
- `unison-cli/src/Unison/LSP/FileAnalysis.hs`
- `unison-cli/src/Unison/LSP/Hover.hs`
- `unison-cli/src/Unison/LSP/Queries.hs`

---

## Remaining Steps (NOT YET DONE)

### Step 8: ANF Conversion

**File:** `unison-runtime/src/Unison/Runtime/ANF.hs`

- Add `LT !TypeTagRepr` to the `Lit` data type (alongside `LY` for TypeLink)
- Add `litRef (LT _) = Ty.typeTagRef`
- Add `anfBlock (TypeTagLit' repr) = pure (mempty, pure . TLit $ LT repr)`

### Step 9: MCode

**File:** `unison-runtime/src/Unison/Runtime/MCode.hs`

- Add `MTT !TypeTagRepr` to `MLit`
- Handle conversion from `LT` to `MTT`

### Step 10: Runtime Foreign Wrapper

**File:** `unison-runtime/src/Unison/Runtime/Stack.hs`

- Add `WrapTypeTag !TypeTagRepr` to the `Foreign` disjoint union
- Add `foreignRef WrapTypeTag{} = Ty.typeTagRef`
- Add `BuiltinForeign TypeTagRepr` instance (wrapBuiltin/maybeUnwrapBuiltin)

### Step 11: Machine Execution

**File:** `unison-runtime/src/Unison/Runtime/Machine.hs`

- Handle `MTT t -> BoxedVal (Foreign (WrapTypeTag t))` in `litToVal` (or equivalent)

### Step 12: Decompiler

**File:** `unison-runtime/src/Unison/Runtime/Decompile.hs`

- Handle `WrapTypeTag repr -> pure $ typeTagLit () repr`

### Step 13: ANF Serialization

**File:** `unison-runtime/src/Unison/Runtime/ANF/Serialize.hs`

- Assign a new tag byte for `LT TypeTagRepr`
- Serialize/deserialize the tree recursively (TTRef=0, TTApp=1, TTArrow=2, TTEffect=3 + recursion)

### Step 14: MCode Serialization

**File:** `unison-runtime/src/Unison/Runtime/MCode/Serialize.hs`

- Handle `MTT TypeTagRepr` serialization

### Step 15: V2 Codebase Format Extension

**File:** `codebase2/codebase/U/Codebase/Term.hs` and `codebase2/codebase-sqlite/`

- Add a `TypeTagLit` variant to the V2 term format
- Update SQLite serialization to store/retrieve TypeTagLit
- Fix the error in `Conversions.hs` to do proper round-tripping

### Step 16: Builtin Operations

**File:** `parser-typechecker/src/Unison/Builtin.hs` (type declarations) and `unison-runtime/src/Unison/Runtime/Builtin.hs` (implementations)

Builtins to add:
```
TypeTag.==          : TypeTag a -> TypeTag b -> Boolean
TypeTag.toText      : TypeTag a -> Text
TypeTag.references  : TypeTag a -> List Link.Type
TypeTag.serialize   : TypeTag a -> Bytes
TypeTag.deserialize : Bytes -> Optional (TypeTag Any)
```

### Step 17: Error Reporting

**File:** `parser-typechecker/src/Unison/PrintError.hs`

- Render "cannot synthesize TypeTag for polymorphic type" with a clear message showing which type variables remain free

### Step 18: Transcript Tests

**File:** `unison-src/transcripts/idempotent/typetag.md`

Test cases:
- Basic synthesis: `f : TypeTag Nat => Nat -> Text`
- Applied types: `TypeTag (List Nat)`, `TypeTag (Map Text Int)`
- Error case: `TypeTag a` where `a` is unresolved
- Runtime inspection after Step 16 is done

---

## Key Architectural Notes

### How the elaboration flow works

1. User writes: `show : TypeTag a => a -> Text`
2. At a call site like `show 42`, the typechecker instantiates `a := Nat` and emits a `ConstraintGoal` for `TypeTag Nat`
3. `GivenElaborator.resolveOne` detects the `TypeTag Nat` pattern via `matchTypeTagGoal`
4. Since `Nat` has no free vars, it produces a synthetic `ResolutionTree` with the sentinel ref `"TypeTag.synth"` and stores `Nat` in `givenConclusion`
5. `GivenApply.buildDictionary` sees the sentinel, calls `typeToRepr` on the conclusion type (`Nat`), and produces `Term.typeTagLit ann (TTRef natRef)`
6. The `TypeTagLit` term is inserted at the call site as the implicit argument

### Type relationships

- `TypeTagRepr` (Haskell ADT) lives in `unison-core/src/Unison/TypeTagRepr.hs`
- At compile time: `TypeTagLit TypeTagRepr` is a constructor of the term functor `F`
- At runtime (once Steps 8-12 are done): `Foreign (WrapTypeTag TypeTagRepr)` on the value stack
- The Unison-facing type is `TypeTag a` (builtin reference `"TypeTag"`, kind `* -> *`)

### Why `TypeTag Any` in synthesizeWanted

When a `TypeTagLit` is encountered during re-typechecking (e.g., loading from codebase), we can't easily recover the original phantom type parameter from the repr. `TypeTag Any` is sound because:
- The phantom parameter is only used for compile-time tracking
- These literals only appear in positions where the elaborator already validated the type
- A more precise approach would reconstruct the type from the repr, but that's unnecessary complexity

### The hashing approach

For content-addressing purposes, `TypeTagLit` hashes by flattening its references (via `typeTagRefs`) and using tag 20 in the `Hashable1 TermF` instance. This is a lossy representation (loses structure) but is sufficient for identity — two TypeTagLits with different structure but same references (impossible in practice for monomorphic types) would collide, but this won't happen because the structure is fully determined by the references for ground types.
