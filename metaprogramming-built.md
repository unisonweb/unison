# Unison metaprogramming — what's actually built

Companion to `metaprogramming-design.md`. The design doc describes
the original target; this doc describes what's on the branch right
now. Where the two diverge, this doc is authoritative.

## Where we deviated from the design

| Design                                    | Built                                                                                                |
|-------------------------------------------|------------------------------------------------------------------------------------------------------|
| Two-layer `Code a` / `Term`                | Single untyped layer: `meta.Term meta.TermF`                                                          |
| Quote / splice syntax `'{ … }` / `${ … }`  | `[| … |]` / `${ … }` — `'{ … }` collides with the existing `'{Ability}` thunk-with-effects syntax     |
| `Codebase` ability + `Codebase.runIO`      | Direct `IO` primitives; users layer their own ability on top if they want one                         |
| `Code.lift` for cross-stage persistence    | No auto-lifting. Splices are explicit. A free variable inside a quote becomes `meta.ABT.Var <name>`. |
| Typed `Type a` witnesses                   | Just `meta.Type meta.TypeF` — same shape as `Term`, untyped at the meta level                         |

The user pivoted toward the untyped Term layer early ("we don't need
a typed Code wrapper") and the rest fell out of that. The typed
`Code a` layer is still a coherent future direction; nothing built
here blocks it.

## Runtime primitives

All live under `Meta.*` and run in `{IO}`. Each has a Prim1 opcode
and is wired through `CCache` for closures that need to call back into
the runtime.

| Builtin                | Type                                                                          | Notes                                                                                                 |
|------------------------|-------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------|
| `Meta.decompile`       | `a -> meta.Term meta.TermF`                                                    | Runtime values → AST. Top-level lambdas expand. Reuses the runtime decompiler.                        |
| `Meta.typecheck`       | `meta.Term meta.TermF -> {IO} Either Text (Link.Term, meta.Term meta.TypeF)`   | Decodes the AST, runs the real typechecker, returns the codebase ref + the inferred type.             |
| `Meta.eval`            | `Link.Term -> {IO} Either Text (Link.Term, meta.Term meta.TypeF)`              | Evaluates a stored term and decompiles the result.                                                    |
| `Meta.load`            | `Link.Term -> {IO} Optional (meta.Term meta.TermF)`                            | Pulls the source AST out of the codebase by reference.                                                |
| `Meta.store`           | `meta.Term meta.TermF -> {IO} Either Text Link.Term`                           | Typechecks, hashes, writes, returns the codebase `Link.Term` (not the intermediate hash).             |
| `Meta.dataDeclShape`   | `meta.Reference -> {IO} Optional [(meta.ConstructorReference, [meta.Term meta.TypeF])]` | Constructor list + per-ctor field type list. Drives the deriving demos.                              |
| `Meta.linkRef`         | `Link.Term -> meta.Reference`                                                  | Extracts the `Reference` out of a `Link.Term`. Backmaps through the runtime's intermediate-hash remap. |

Helpers in `lib.meta`:

| Helper        | Behavior                                                                                                 |
|---------------|----------------------------------------------------------------------------------------------------------|
| `Meta.run`    | Typecheck + evaluate a quoted term and return the value, without writing to the codebase.                |
| `mark.given`  | UCM command. Marks a stored term as `given`, so the implicit resolver considers it during elaboration.   |

## Surface syntax

### Quote: `[| e |]`

Lifts a Unison expression `e` to a `meta.Term meta.TermF` value. The
parser desugars the body into raw `meta.Term` / `meta.ABT` /
`meta.TermF` constructors. Supported expression forms:

* literals (`Nat`, `Int`, `Float`, `Boolean`, `Text`, `Char`)
* lambdas — `[| x -> ... |]` (HOAS; the binder is in scope inside the quote)
* application
* references — codebase definitions resolve to `meta.TermF.Ref`, constructors to `meta.TermF.Constructor`, requests to `meta.TermF.Request`
* `if / then / else`, `&&`, `||`
* `handle … with …`
* `let` / `let rec` blocks
* `match … with … | cases …` — all pattern forms except effect patterns

A free variable inside a quote that isn't a codebase reference is
emitted as a `meta.ABT.Var` carrying the bare name. This is what
makes the Oleg-style staged power example work: the recursive
`power n y` references its parameter `y` by name across stages.

### Splice: `${ e }`

Inserts a meta-Term expression into the surrounding quote. The
expression `e` must evaluate to a `meta.Term meta.TermF` (or the
relevant sub-shape). At runtime `${ e }` is wrapped in a
`meta.splice : forall a. a -> a` builtin so the printer can recover
the source form — at the value level it's identity.

### Round-trip

`view` of any quoted term reconstructs `[| … |]` from the desugared
form, including splice markers. So a stored macro shows up as
source the way it was written, not as a constructor tree.

## What's in `meta.Term` / `meta.TermF` / friends

The Unison-side types are defined in `unison-runtime/src/Unison/Runtime/MetaSource.hs`
with pinned hashes. They mirror the Haskell ABT closely:

```
type Term f = Term (Set Name) (ABT f (Term f))
type ABT f a = Var Name | Abs Name a | Cycle a | Tm (f a)
type TermF a = App a a | Lam a | Let a a | LetRec [a] a
             | If a a a | Match a [MatchCase a] | Handle a a | Ann a (Term TypeF)
             | Ref Reference | Constructor ConstructorReference | Request ConstructorReference
             | Sequence [a] | Lit Literal
type TypeF a = …   -- arrows, foralls, abilities, refs, vars
type Pattern = PUnbound | PVar | PBoolean Bool | … | PConstructor Reference Nat [Pattern] | …
```

## Transcripts

In suggested reading order:

| Transcript                                | What it shows                                                            |
|-------------------------------------------|--------------------------------------------------------------------------|
| `meta-quote.md`                           | Basic quoting + splicing + free-var handling.                            |
| `meta-quote-match.md`                     | Pattern-matching inside quotes (constructor, literal, As, guard, sequence). |
| `meta-quote-let.md`                       | `let`, `let rec`, `if`, `&&`, `||`, `handle` inside quotes.              |
| `meta-quote-power.md`                     | Oleg-style staged power function; cross-stage variable binding.          |
| `meta-quote-run.md`                       | `Meta.run` library helper end-to-end.                                    |
| `meta-typecheck.md` / `meta-load.md` / `meta-store.md` | The runtime IO primitives in isolation.                                  |
| `meta-derive-given.md`                    | Macro deriving a Show instance and marking it `given`.                   |
| `meta-derive-functor.md`                  | Deriving Functor for Optional.                                           |
| `meta-derive-functor-recursive.md`        | Deriving Functor for `Tree a = Leaf | Node a (Tree a) (Tree a)` via meta-letrec. |
| `meta-derive-functor-nested.md`           | Deriving Functor for `Search a = Found a | Continue (Optional (Search a))` — fmaps through `Optional`. |

## Not built (yet)

* **Effect patterns inside quotes** — `{ Foo.bar x -> … }`. The desugarer raises a clear parser error.
* **Sequence / list literals inside quotes** — `[| [1, 2, 3] |]`. Falls through as a free splice today.
* **`Code a` typed wrapper.** Everything is `meta.Term meta.TermF`; the typed layer is future work.
* **`Codebase` ability.** Users compose one over the `Meta.*` IO primitives.
* **Auto-lift / cross-stage persistence.** Splices are explicit; the existing free-var-in-quote → `meta.ABT.Var` rule isn't a substitute, but it covers the common staging-by-name case.

## File-level pointers

* `unison-runtime/src/Unison/Runtime/MetaSource.hs` — the embedded Unison source for the meta types.
* `unison-runtime/src/Unison/Runtime/MetaCompile.hs` — decoder from runtime meta-Term values → real `Term` AST.
* `unison-runtime/src/Unison/Runtime/Interface.hs` — runtime installers for the `Meta.*` builtins (search for `metaTC`, `metaStoreF`, `metaLinkRefF`, etc.).
* `parser-typechecker/src/Unison/Syntax/TermParser.hs` — `desugarQuote` and friends (`metaPattern`, `lowerCase`, `wrapMetaAbs`).
* `parser-typechecker/src/Unison/Syntax/TermPrinter.hs` — `toQuotedSource` does the printer-side round-trip.
* `parser-typechecker/src/Unison/Builtin.hs` — `Meta.*` builtin signatures + the `meta.splice` marker.
* `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` — lexer entries for `[|`, `|]`, `${`.
