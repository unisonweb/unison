# Unison metaprogramming — design synthesis

## Goals
- Unison programs can construct, evaluate, inspect, rewrite, and store Unison code.
- Typed quotation for the in-program case: `eval` is total.
- Untyped structural representation for the load/rewrite/store case, with typecheck-gated promotion to the typed layer.
- Hygiene by construction (HOAS).
- Composes with hash-addressing: rewriting yields a new reference, the old one stays.

## Non-goals (v1)
- Typed quoted patterns à la Scala 3. Patterns live on `Term`.
- Compile-time macros. Everything runs at runtime, behind an ability.
- Source-text manipulation. The surface is ABT, not strings.

## The four types

```
Code a     -- typed, opaque, runnable, hash-stable
Term       -- untyped ABT, structural, pattern-matchable
Type a     -- reified type witness, parameterized by Unison type a
Reference  -- hash, content-address into the codebase
```

## Surface syntax

Two new parser forms:

```
'{ e }     -- quote: Code a where a is inferred from e
${ e }     -- splice: e : Code a inserts at a hole of type a
```

Inside a quote, free identifiers from the enclosing scope are auto-lifted via `Code.lift` (cross-stage persistence). HOAS for binders: `'{ x -> x + 1 }` is parsed as a `Lam` whose body is built with `x` bound as a Unison binder. Splices are only legal inside quotes, at positions where the expected type is known.

Dropping to `Term` has no syntax — use `Code.toTerm` explicitly.

## Operations on `Code a` (typed layer)

```
Code.eval         : Code a ->{Abilities of a} a
Code.lift         : a -> Code a                          -- derivable for serializable types
Code.toTerm       : Code a -> Term
Code.type         : Code a -> Type a
Code.dependencies : Code a -> Set Reference
```

`Code.eval` is total. Abilities required at the call site are exactly the abilities appearing in `a` — reuses Unison's existing row machinery the way `unsafeRun!` does today.

No `Code.fromTerm` without a `Type a` argument, and no unchecked cast analogous to Scala's `asExprOf[T]`. Every promotion from `Term` to `Code a` goes through a real typecheck.

## Operations on `Term` (untyped layer)

```
type Term
  = Var Name
  | Abs Name Term
  | App Term Term
  | Lam Term                                  -- body is an Abs
  | Let Term Term                             -- second is an Abs
  | LetRec [Term] Term
  | If Term Term Term
  | Match Term [MatchCase]
  | Ref Reference
  | Constructor ConstructorReference
  | Request ConstructorReference
  | Handle Term Term
  | Ann Term TypeTerm
  | Lit Literal
  | List [Term]
```

Standard ABT machinery:

```
Term.freeVars    : Term -> Set Name
Term.subst       : Name -> Term -> Term -> Term      -- capture-avoiding
Term.rename      : Name -> Name -> Term -> Term
Term.alphaEq     : Term -> Term -> Boolean
Term.mapChildren : (Term -> Term) -> Term -> Term
```

Pattern matching is ordinary Unison matching on the `Term` ADT.

## Operations on `Type a`

```
Type.eq      : Type a -> Type b -> Optional (a === b)    -- equality with coercion evidence
Type.show    : Type a -> Text
Type.toTerm  : Type a -> TypeTerm
Type.parse   : Text -> Either ParseError (∃a. Type a)
```

`a === b` is Leibniz equality so `Type.eq` returns enough to safely coerce a value of one type to the other when they match.

## Codebase access: IO primitives + library ability

Codebase access is exposed as a minimal set of `Meta.*` IO primitives. The runtime stays small, alternative backends (mock for tests, in-memory, sandboxed view) are user-space concerns, and this matches the convention the existing `Meta.*` builtins already follow.

The primitives:

```
Meta.load       : Reference -> {IO} Optional Term
Meta.loadType   : Reference -> {IO} Optional (∃a. Type a)
Meta.store      : Term -> {IO} Either TypeError Reference
Meta.lookup     : Name -> {IO} Optional Reference
Meta.dependents : Reference -> {IO} Set Reference
Meta.namespace  : Path -> {IO} List (Name, Reference)
```

`Meta.store` typechecks, walks the transitive `Reference` closure for reachability (same machinery as `Code.serialize`), then hashes the normalized typed form. Equivalent terms get the same hash — α-normalization already handles this.

A typed `Codebase` ability is then a pure-Unison library on top:

```
ability Codebase where
  load        : Reference -> Term
  store       : Term -> Either TypeError Reference
  lookup      : Name -> Optional Reference
  dependents  : Reference -> Set Reference

Codebase.runIO : '{Codebase, IO} a ->{IO} a
Codebase.runIO c = handle !c with cases
  { Codebase.load r       -> k } -> Codebase.runIO '(k (Meta.load r |> Optional.getOrElse (bug "not found")))
  { Codebase.store t      -> k } -> Codebase.runIO '(k (Meta.store t))
  { Codebase.lookup n     -> k } -> Codebase.runIO '(k (Meta.lookup n))
  { Codebase.dependents r -> k } -> Codebase.runIO '(k (Meta.dependents r))
  { a } -> a
```

Users can write `Codebase.runMock`, `Codebase.runReadOnly`, `Codebase.runSnapshot`, etc., as ordinary handlers. Programs that prefer not to use the ability can call `Meta.*` directly in `IO`.

## The bridge

```
Term.typecheck   : Term -> {Codebase} Either TypeError (∃a. (Type a, Code a))
Term.typecheckAs : Type a -> Term -> {Codebase} Either TypeError (Code a)
Code.toTerm      : Code a -> Term
```

## The killer workflow

Either flavor works — pick by whether the caller wants a testable ability surface or direct IO.

```
-- ability-flavored, mockable
rewriteAll : (Term -> Term) -> Reference ->{Codebase, Throw TypeError} Reference
rewriteAll f r =
  match Codebase.store (f (Codebase.load r)) with
    Left e  -> throw e
    Right r -> r

-- IO-flavored, direct
rewriteAllIO : (Term -> Term) -> Reference ->{IO} Either Text Reference
rewriteAllIO f r = match Meta.load r with
  None   -> Left "not found"
  Some t -> match Meta.store (f t) with
    Left e  -> Left (typeErrorText e)
    Right r -> Right r
```

Load, rewrite as ordinary pure Unison, store. New hash, old hash persists, dependents migrate via existing `ucm update` machinery. This is the design's reason to exist.

## Examples

Peephole optimizer (pure Term):

```
optimize : Term -> Term
optimize = cases
  Term.App (Term.App (Term.Ref +Ref) x) (Term.Lit (Nat 0)) -> x
  Term.App (Term.Lam (Term.Abs n body)) x                  -> Term.subst n x body
  t                                                         -> Term.mapChildren optimize t
```

Staged interpreter (typed Code):

```
compile : Program -> Code (Input -> Output)
compile p = '{ input -> ${ compileBody p } input }
```

Definition migrator (combines both layers):

```
migrate : Name -> Name -> {Codebase} Optional Reference
migrate from to = match Codebase.lookup from with
  None   -> None
  Some r -> Some (rewriteAll (renameRef from to) r)
```

## Hard sub-questions, surfaced not hidden

1. **What's in `Term`.** Decompiled runtime values are a strict subset of source terms. Pick: (a) union — `Term` covers arbitrary source, decompiled values are a subset; (b) intersection — `Term` covers only what round-trips compile/decompile. Recommend (a); (b) is too restrictive for rewriters.

2. **Abilities under quotation.** `'{ x -> Console.print x; x + 1 }` should infer `Code (Nat ->{Console} Nat)`. The quote elaborator runs the full type inferencer; no shortcuts. Confirm row-polymorphism composes through splice holes the way you'd want.

3. **Splice type-mismatch errors.** A splice whose ability row is wider than the hole's expected row must error at quote elaboration, not at `eval`. Quote elaboration is type-checking, not a syntactic transform.

4. **References in constructed terms.** Splicing a `Code` into a `Term` (or `App`-ing a `Term.Ref`) embeds a `Reference`. `store` must validate the closure is reachable in the current codebase view. Reuse `Code.serialize`'s dependency walker.

5. **Naming inside quotes.** `'{ x -> ... }` introduces `x` as a binder lexically scoped to the quote. This is the only new variable-binding semantics in the parser. Specify shadowing, suggestion behavior, and error messages tightly so `'{ ${someBoundVar} }` vs `'{ someFreeName }` is unambiguous.

6. **Hash stability of rewrites.** `optimize` of α-equivalent inputs must produce α-equivalent (hash-equal) outputs. Existing hashing normalizes α; verify rewriters don't construct terms with stale binder metadata that defeats normalization.

7. **`Type` ADT scope.** `Type` needs constructors for: function arrows with ability rows, data type refs, foralls, ability rows themselves, type constants, type variables. This is its own small ABT. Probably reuses the same ABT infrastructure as `Term`.

## Implementation order

1. **`Term` ADT + ABT ops in Unison.** Pure port from Haskell. No language changes. Test α-equivalence, capture-avoiding substitution, freshening.
2. **Decompile primitive: `decompile : a -> Term`.** Wires to the existing runtime decompiler.
3. **`Type a` reification** + `Type.eq`, `Type.show`. Smallest piece that unlocks the rest.
4. **`Term.typecheck`** as a runtime primitive backed by the existing typechecker.
5. **Codebase access.**
   - 5a. **`Meta.*` IO primitives** for load/store/lookup/dependents/namespace. `Meta.store` goes through typecheck + hash + dependency walk. Small runtime change, unlocks the killer workflow on its own.
   - 5b. **`lib.Codebase` ability** (pure Unison library) on top of the IO primitives, plus `Codebase.runIO` and mock/snapshot handlers. Ships independently and at any time after 5a.
6. **Parameterize existing `Code` builtin by type.** Add `Code.eval`, `Code.lift`, `Code.toTerm`.
7. **Quote/splice syntax** in the parser + elaborator. Elaboration runs the inferencer over the quoted expression, threading variables through splice holes.
8. **Derive `Code.lift`** for all serializable types.

Steps 1–5 are self-contained — they unlock the load/rewrite/store workflow with zero new syntax, using ordinary Unison values of type `Term`. Steps 6–8 are the typed-quotation layer on top, separable in time. Ship 1–5 first, learn from it, then decide whether 6–8 needs adjustment.
