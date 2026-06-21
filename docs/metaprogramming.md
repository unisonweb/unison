# Unison metaprogramming

Unison programs can construct, evaluate, introspect, rewrite, and
store other Unison programs. This document is an overview of the
machinery: the surface syntax for quoted code, the meta types it
denotes, and the `Meta.*` builtins that bridge the two.

## Two layers

There are two layers, related by encode/decode:

- **The runtime layer** — ordinary Unison values. Lambdas, ADT
  constructors, references, `Link.Term`s, the works.
- **The meta layer** — Unison data types in the `meta` namespace
  (`meta.Term`, `meta.TermF`, `meta.TypeF`, `meta.ABT`,
  `meta.Reference`, `meta.Pattern`, …) that mirror the underlying
  AST. A value of type `meta.Term meta.TermF` is an AST node,
  manipulable with ordinary pattern matching.

`Meta.decompile` lifts from runtime → meta. `Meta.store`/`Meta.eval`
go the other way (with typechecking).

## Surface syntax

### Quote — `[| e |]`

Lifts a Unison expression to a `meta.Term meta.TermF` value. The
parser walks the expression and desugars it into the corresponding
`meta.Term` / `meta.ABT` / `meta.TermF` constructors at compile time.

Supported expression forms inside a quote: literals (`Nat`, `Int`,
`Float`, `Boolean`, `Text`, `Char`); lambdas (HOAS — the binder is
in scope inside the quote); application; references and
constructors (codebase definitions resolve to `meta.TermF.Ref` /
`Constructor` / `Request`); `if`/`then`/`else`, `&&`, `||`;
`handle … with …`; `let` and `let rec` blocks; `match … with …`
including the `cases` shorthand. All pattern forms except effect
patterns are supported.

A free variable inside a quote that doesn't resolve to a codebase
reference is emitted as `meta.ABT.Var` carrying the bare name. This
is what makes staged programming — like the classic Oleg-style
`power` example — work: the recursive call refers to its parameter
by name across stages.

### Splice — `${ e }`

Plugs a `meta.Term meta.TermF` value into the surrounding quote at
that position. The spliced expression must evaluate to a meta-term
of the matching shape. At runtime the splice is wrapped in
`Meta.splice` (an identity function) so the printer can recover
the source form when round-tripping.

### Round-trip

`view` of any term whose body is a quote reconstructs the
`[| … |]` source from the desugared form, including splices —
stored macros show up as source the way they were written, not as
raw constructor trees.

## Runtime primitives

All live under `Meta.*` and run in `{IO}`. Each has a Prim1 or
Prim2 opcode and is wired through `CCache` for closures that need
to call back into the runtime.

| Builtin              | Type                                                                                  |
|----------------------|---------------------------------------------------------------------------------------|
| `Meta.decompile`     | `a -> {IO} meta.Term meta.TermF`                                                       |
| `Meta.typecheck`     | `meta.Term meta.TermF -> {IO} Either Text (meta.Term meta.TypeF, Link.Term)`           |
| `Meta.eval`          | `Link.Term -> {IO} Either Text (meta.Term meta.TypeF, Link.Term)`                      |
| `Meta.load`          | `Link.Term -> {IO} Optional (meta.Term meta.TermF)`                                    |
| `Meta.store`         | `meta.Term meta.TermF -> {IO} Either Text Link.Term`                                   |
| `Meta.dataDeclShape` | `meta.Reference -> {IO} Optional [(meta.ConstructorReference, [meta.Term meta.TypeF])]`|
| `Meta.linkRef`       | `Link.Term -> meta.Reference`                                                          |
| `Meta.splice`        | `forall a. a -> a` (identity — splice round-trip marker)                               |
| `Meta.aliasTerm`     | `Link.Term -> Text -> {IO} ()`                                                         |
| `Meta.aliasType`     | `Link.Type -> Text -> {IO} ()`                                                         |
| `Meta.deleteTerm`    | `Text -> {IO} ()`                                                                      |
| `Meta.moveTerm`      | `Text -> Text -> {IO} ()`                                                              |
| `Meta.lookup`        | `Text -> {IO} Optional Link.Term`                                                      |
| `Meta.dependents`    | `Link.Term -> {IO} [Link.Term]`                                                        |

`Meta.decompile` lifts a runtime value to its AST. `Meta.store`
typechecks an AST, hashes the result, and writes it to the
codebase, returning the new `Link.Term`. `Meta.eval` evaluates a
stored term and decompiles the result. `Meta.load` is the inverse
of `Meta.store` — pulls an AST back out of the codebase.

`Meta.dataDeclShape` introspects a data type's constructors and
field types — the substrate for type-shape-driven derivers like
`Functor`. `Meta.linkRef` extracts a `meta.Reference` from a
`Link.Term`, useful for embedding hash-addressed references inside
quoted code.

The `aliasTerm`/`aliasType`/`deleteTerm`/`moveTerm`/`lookup`/
`dependents` builtins are UCM's analogous commands available from
inside `{IO}`. Mutating calls queue actions during evaluation and
are applied via the standard branch-mutation machinery once the
program returns to the CLI, so SQLite/LSP/check-and-set behave
identically to the equivalent typed-in UCM command.

For the opposite direction of `dependents` — "what does this
*depend on*" — use the existing `Code.dependencies` builtin on a
cached `Code` value.

## Meta types

The Unison-side types live under the `meta` namespace, defined in
`unison-runtime/src/Unison/Runtime/MetaSource.hs` with pinned
hashes:

```
type Term f = Term (Set Name) (ABT f (Term f))
type ABT f a = Var Name | Abs Name a | Cycle a | Tm (f a)
type TermF a
  = App a a | Lam a | Let a a | LetRec [a] a
  | If a a a | Match a [MatchCase a] | Handle a a | Ann a (Term TypeF)
  | Ref Reference | Constructor ConstructorReference | Request ConstructorReference
  | Sequence [a] | Lit Literal
type TypeF a
  = TypeRef Reference | TypeArrow a a | TypeApp a a | TypeForall a
  | TypeEffect a a | TypeEffects [a] | TypeIntroOuter a | TypeTypeVar Name
type Pattern
  = PUnbound | PVar | PBoolean Boolean | PInt Int | PNat Nat | PFloat Float
  | PText Text | PChar Char | PBytes Bytes | PConstructor Reference Nat [Pattern]
  | PAs Pattern | PEffectPure Pattern | PEffectBind Reference Nat [Pattern] Pattern
  | PSequenceLiteral [Pattern] | PSequenceOp Pattern SeqOp Pattern
```

The hashes are pinned at the GUID level (`unique[…]`) — changing
the shape of any of these types changes the hash and invalidates
every codebase reference that uses them. Evolve them in lockstep
with the `MetaSource` source and the matching Haskell wrappers in
`MetaCompile` / `MetaDecompile`.

## Transcripts

A reading order, easiest to most demanding:

| Transcript                            | What it covers                                              |
|---------------------------------------|-------------------------------------------------------------|
| `meta-tour.md`                        | Whirlwind tour through the whole API.                       |
| `meta-quote.md`                       | Basic quoting, splicing, free-var handling.                 |
| `meta-quote-match.md`                 | Pattern matching inside quotes.                             |
| `meta-quote-let.md`                   | `let`, `let rec`, `if`, `handle` inside quotes.             |
| `meta-quote-power.md`                 | Oleg-style staged `power` — cross-stage variable binding.   |
| `meta-typecheck.md` / `meta-load.md` / `meta-store.md` | The IO primitives in isolation.            |
| `meta-rewrite.md`                     | The full load → transform → store workflow.                 |
| `meta-ucm-builtins.md`                | All six UCM-as-builtins in one read-through.                |
| `meta-derive-functor-nested.md`       | Deriving Functor for `Search a = Found a \| Continue (Optional (Search a))` — recursion + nested instances. |
| `meta-futamura.md`                    | The Futamura projections — staged DSL compiler in 30 lines. |

## What's not built

- **Effect patterns inside quotes** — `{ Foo.bar x -> … }` raises a
  clear parser error.
- **Typed `Code a` wrapper.** Everything is `meta.Term meta.TermF`;
  a typed quotation layer (à la Scala 3 / MetaOCaml) is future work
  that could be built on top.
- **Auto-lift / cross-stage persistence.** Splices are always
  explicit. The free-var-in-quote → `meta.ABT.Var` rule covers the
  common staging-by-name case but isn't a substitute.

## File pointers

- `unison-runtime/src/Unison/Runtime/MetaSource.hs` — the embedded
  Unison source for the meta types, with pinned hashes.
- `unison-runtime/src/Unison/Runtime/MetaDecompile.hs` — runtime
  value → `meta.Term` value (the `Meta.decompile` substrate).
- `unison-runtime/src/Unison/Runtime/MetaCompile.hs` — `meta.Term`
  value → real `Term` AST + typechecker dispatch (the
  `Meta.typecheck` / `Meta.store` substrate).
- `unison-runtime/src/Unison/Runtime/Interface.hs` — runtime
  installers for the `Meta.*` builtins (search for `metaTC`,
  `metaStoreF`, `metaAliasTermF`, etc.).
- `parser-typechecker/src/Unison/Syntax/TermParser.hs` —
  `desugarQuote` and the helpers it depends on.
- `parser-typechecker/src/Unison/Syntax/TermPrinter.hs` —
  `toQuotedSource` does the printer round-trip for `[| … |]`.
- `parser-typechecker/src/Unison/Builtin.hs` — `Meta.*` builtin
  type signatures and `Meta.splice`.
- `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` — lexer
  entries for `[|`, `|]`, `${`.
