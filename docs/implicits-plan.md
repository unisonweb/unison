# Implicit parameters for Unison — design and execution plan

**Status:** draft / pre-RFC. Working name: `givens`.

This document captures the design and the staged execution plan for adding
Scala-3-style `given`/`using` implicit parameters to Unison. It is the source
of truth that the ADRs in `docs/architecture/decisions/` (to be created in
Phase 0) will refine.

---

## 1. Design summary

### 1.1 Guiding principles

Five things are load-bearing for the design "fitting like a glove":

1. **Givens are sugar for ordinary parameters.** After elaboration, a
   constraint parameter is an ordinary positional argument. Runtime,
   codegen, ABT, and hashing learn no new concept. The whole feature lives
   in the elaborator.

2. **Resolution is baked into the hash.** A term that referenced a
   constraint resolves the dictionary at typecheck time and freezes the
   chosen hash into the term. Old terms never change instance — content
   addressing makes implicits coherent for already-elaborated code by
   construction.

3. **Namespace = instance set.** The visible givens in your current
   namespace *are* your instance dictionary. No orphan rules, no global
   registry. Two libraries that each define `given Show.nat` produce an
   ambiguity at any call site that sees both, surfaced through the same
   machinery TDNR uses today.

4. **Givenness is a namespace-level tag on a hash.** Marking a definition
   `given` is recorded in namespace metadata, not in the term itself. The
   underlying *term hash* is unchanged by tagging — aliases, dependent
   terms, and history pointers all keep working. The *branch (causal)
   hash* of the namespace does change, since it summarises namespace
   contents including metadata. A downstream user can promote an upstream
   definition to a given locally, or demote an upstream given they don't
   want, all without touching term hashes.

5. **Givens extend TDNR rather than replace it.** TDNR resolves "which
   named definition fits this type"; given resolution adds "which value of
   this type exists, with chaining." Same elaborator, same ambiguity
   reporting, same "did you mean…" affordances.

### 1.2 Surface syntax

#### Declaring a "class"

There is no class declaration. A class is just a type — typically a record.

```unison
unique type Show a = Show { show : a -> Text }
unique type Ord a  = Ord  { compare : a -> a -> Ordering, eq : Eq a }
unique type Functor f = Functor { fmap : ∀ a b. (a -> b) -> f a -> f b }
unique type Monad m   = Monad   { pure : ∀ a. a -> m a
                                , bind : ∀ a b. m a -> (a ->{} m b) ->{} m b
                                , functor : Functor m }
```

Subclassing falls out of records-containing-records.

#### Declaring a given

```unison
given Show.nat : Show Nat = Show Nat.toText

given Show.list : Show a => Show (List a) =
  Show (xs -> "[" ++ Text.join ", " (List.map Show.show xs) ++ "]")

given Functor.optional : Functor Optional = Functor Optional.map
```

The keyword `given` is a namespace tag: it records that this hash is
eligible for implicit resolution in this namespace. `Show a => Show (List
a)` desugars to `Show a -> Show (List a)` with the first parameter marked
as a `using` parameter.

#### Consuming givens

```unison
print : Show a => a ->{IO} ()
print a = printLine (Show.show a)

sort : Ord a => [a] -> [a]

mapM : (Monad m, Traversable t) => (a ->{e} m b) -> t a ->{e} m (t b)
```

#### Local givens

```unison
sortReversed : Ord a => [a] -> [a]
sortReversed xs =
  given local : Ord a = Ord.flip (summon (Ord a))
  sort xs
```

Local givens shadow ambient ones (lexically inner wins).

#### Explicit summon and override

`summon T` is the explicit summon expression — rare in practice because
constraint arguments are inserted automatically at every call. Used when
the dictionary itself is needed as a value.

`f @ d x` passes `d` explicitly to the next implicit slot of `f`, useful
for testing and for breaking ambiguity without introducing a local given.

### 1.3 Resolution algorithm

```
resolve(T, stack, depth) =
  if depth > maxDepth: error DepthExceeded(stack)
  if any(unifies(T, S)) for S in stack: fail this branch (cycle)

  candidates = { g ∈ visibleGivens
               | conclusion(g) unifies with T under σ }

  results = []
  for (g, σ) in candidates:
      premises = σ(premises(g))
      try:
          subs = [resolve(p, stack ∪ {T}, depth+1) for p in premises]
          results += (g, σ, subs)
      catch: continue   -- this candidate didn't pan out

  case results of
    []  → error NoGiven(T, near = nearMisses(T))
    [r] → r
    rs  → case mostSpecific(rs) of
            Just r  → r
            Nothing → error Ambiguous(rs)
```

#### Specificity ordering
- **Subsumption.** A's conclusion is strictly more specific than B's iff
  there is a substitution σ with σ(B) = A and σ ≠ identity.
- **Lexical proximity.** Local `given` beats outer `given` regardless of
  type specificity.
- **Otherwise tie.** Report ambiguity with both candidates.

#### Cycle and depth handling
- Cycles detected per branch via `stack`. Hitting a cycle fails *that
  candidate*; other candidates may still succeed.
- Depth limit global per top-level resolution. Default 50, configurable
  via project setting. Hitting the limit always errors.
- The error reports the chain.

### 1.4 What's *not* in v1

- Functional dependencies / associated types.
- Implicit conversions (givens fill explicit holes only).
- Default parameters.
- Macro/derivation (`deriving Show`).
- Anonymous given lambdas / first-class implicit-function types.
- **Kind-polymorphic givens.** Unison's kind system has only `Type`,
  `Ability`, and arrow kinds (`Kind :-> Kind`). There is no kind
  polymorphism. Givens that abstract over kinds (rare —
  `Bifunctor`-shaped premises with kind variables) won't work in v1.
  Standard HKT — `Functor f`, `Monad m` where `f, m :: Type -> Type` —
  works fine.

---

## 2. Phasing at a glance

```
Phase 0  Design freeze      ADRs ratified, RFC drafted
Phase 1  Spike              throwaway prototype validates resolution
Phase 2  Core               parser, namespace, typechecker, elaborator
Phase 3  UCM integration    view/find/edit/update for givens
Phase 4  Pretty + errors    polished diagnostics, printer modes
Phase 5  Stdlib seed        Show, Ord, Functor, Monad, Traversable
Phase 6  Release            RFC merged, behind a flag → on by default
```

Each phase has a gate that must close before the next opens. Phase 2's
subprojects run partly in parallel, gated internally.

---

## 3. Phase 0 — Design freeze

**Goal:** every contentious decision recorded as an ADR and ratified by
core maintainers. No code yet.

### 3.1 ADRs to author

| #   | Title                                                                  | Status     |
|-----|------------------------------------------------------------------------|------------|
| 001 | Constraint syntax: Haskell-style `C =>`                                | settled    |
| 002 | Givenness is namespace metadata, not a term-level tag                  | settled    |
| 003 | A "class" is an ordinary type; no new declaration kind                 | settled    |
| 004 | Resolution is purely compile-time; runtime/codegen untouched           | settled    |
| 005 | Coherence: ambiguity at call site, no global uniqueness                | settled    |
| 006 | `summon T` is the explicit summon form                                 | settled    |
| 007 | `@`-positional explicit override at call sites                         | settled    |
| 008 | Specificity ordering: subsumption + lexical inner > outer              | settled    |
| 009 | Cycle detection per branch; depth limit global, default 50             | settled    |
| 010 | Local `given` in `let`-blocks; no first-class implicit functions       | settled    |
| 011 | HKT support in v1; functional dependencies and assoc types deferred    | settled    |
| 012 | Where the elaborator pass sits relative to inference                   | open       |
| 013 | Storage format for a namespace's given-set (and how it's diffed/pushed)| open       |
| 014 | Hash treatment of given declarations and elaborated terms              | open       |
| 015 | Pretty-printer default: elide implicit args; verbose mode shows them   | open       |
| 016 | Update semantics when a re-typecheck would re-resolve differently      | open       |
| 017 | UCM surface: new commands and modifications to existing ones           | open       |
| 018 | Feature flag rollout: gated by project config until stable             | open       |
| 019 | Representation of implicit arrows in `Type.F` (extend / side-table / wrapper) | **open, blocking** |
| 020 | Metadata serialization format and migration from dormant `causal_metadata` table | **open, blocking** |
| 021 | Merge semantics for given-set conflicts in `unison-merge`              | open       |
| 022 | Keyword migration plan for `given`/`summon` (currently legal identifiers) | open    |
| 023 | Memoization scope and cache key for resolution (Phase-1 spike output) | open       |

### 3.2 Phase 0 deliverables
- All 18 ADRs in `docs/architecture/decisions/`.
- A public RFC issue/PR linking to the ADR set, soliciting community input
  for ~2 weeks.
- A "what doesn't change" companion doc that codifies the
  runtime/hashing/codegen non-changes.

### 3.3 Gate to Phase 1
- All ADRs marked *Accepted* (not *Proposed*).
- RFC has at least one round of feedback addressed.
- Two core maintainers sign off.
- ADRs **012, 013, 014, 016, 019, 020** specifically have *no open
  questions*. These six block the spike. ADR 019 is foundational because
  the "settled" ADR 004 (runtime/codegen untouched) presupposes its
  answer; ADR 020 is foundational because the dormant `causal_metadata`
  table forces a choice on serialization shape.

---

## 4. Phase 1 — Spike

**Goal:** prove the resolution algorithm works on a toy implementation, in
a throwaway branch, *without touching the parser or main typechecker.*

### 4.1 Scope
- A standalone Haskell module that takes a hand-written AST of
  "elaboration goals" plus a "given pool" and runs the resolution
  algorithm.
- Inputs: list of mock givens with type-shaped premises and conclusions,
  list of constraint goals.
- Output: resolution tree or error.
- Test harness covering chaining, HKT, ambiguity, cycles, depth blowup.

### 4.2 Success criteria
1. Resolution terminates on cycles (per-branch detection).
2. Specificity ordering produces the expected winner on `Show (List Nat)`
   vs `Show (List a)`.
3. HKT cases resolve: `Functor List`, `Monad Optional`.
4. Ambiguity errors include both candidates with enough context.
5. Performance acceptable for ~1000 givens with ~20-deep chains.
6. **Diamond dependencies don't blow up.** Resolution of compound
   instances like `Show (Map (List a) Nat)` — where the same sub-goal
   (`Show Nat`) is reached by multiple chain paths — completes without
   exponential blowup or duplicate sub-resolution. Memoize sub-results by
   type within a single top-level resolution.

### 4.3 Out of scope
- No parser changes. AST is hand-built.
- No real Unison types — mock representation just rich enough for
  unification + HKT.
- No TDNR integration.
- No namespace concept; givens flat list.

### 4.4 Gate to Phase 2
- All six spike-success criteria demonstrated.
- A 1-page writeup: what we learned, what surprised us, what we now know
  we got wrong in the ADRs.
- *If the spike surfaces an ADR-level issue, return to Phase 0.*

---

## 5. Phase 2 — Core implementation

Six subprojects. Numbered in topological order, but 2.B and 2.C overlap
heavily and 2.E starts as soon as 2.D has a working stub. 2.C is split
into 2.C.1 (foundational, blocks 2.D) and 2.C.2 (parallel with 2.D).

### 5.1 Subproject 2.A — Parser & AST extensions
- Lex/parse `=>` in type signatures.
- Lex/parse `given` as a definition prefix.
- Lex/parse `given` in `let`-blocks.
- Lex/parse `summon T` expression.
- Lex/parse `@`-positional explicit-override at call sites.
- AST nodes for: constraint-bearing function types, given declarations,
  summon expressions, implicit-application override.
- No semantics yet — parser produces nodes, downstream phases ignore them.

**Exit:** parser round-trips every example in the RFC. Snapshot tests for
both success and helpful parse errors.

### 5.2 Subproject 2.B — Namespace given-set storage
- Schema change: a namespace tracks `Set Hash` of givens. Likely fits in
  the existing `MdValues` slot in `U.Codebase.Branch.Type` rather than a
  new schema field — pending ADR 020.
- Codebase format: per ADR 020, decide between (a) reviving the dormant
  `causal_metadata` table, (b) reusing `MdValues` with a sentinel
  built-in `Reference` like `##Builtin.Given`, or (c) a new field on
  branch serialization.
- Migration: existing namespaces have empty given-sets.
- Push/pull: given-set travels with its namespace. Sharing servers learn
  the new field; bump the relevant API version in `unison-share-api` and
  `unison-share-projects-api`. Coordinate with a server release.
- Aliasing/move/delete: hash being given-tagged in namespace X doesn't
  imply given in namespace Y. Aliases inherit by default; rename within a
  namespace preserves givenness.
- **Merge semantics (per ADR 021):** extend `unison-merge` with
  given-set conflict resolution. Cases to define: both branches mark a
  hash given (no-op), one marks and the other doesn't (mark wins —
  user-confirmable), both branches rename the same given to different
  names (existing rename-conflict path, with givenness preserved on the
  resulting hash).
- **Plumbing for read sites.** `MdValues` schema exists but is not
  currently read by user-facing flows (`grep -rn MdValues unison-cli`
  returns nothing). Wire it through name lookup, project APIs, and any
  command that surfaces definitions.

**Exit:** existing UCM round-trip tests pass with no diff. New round-trip
tests cover marking, unmarking, aliasing, push/pull, fork, **merge with
given-set conflicts**. Sharing API version bumped and a coordinated
server build available.

### 5.3 Subproject 2.C.1 — Type-AST representation of implicit arrows

**Blocks 2.D.** Per ADR 019, choose how implicit-ness is represented in
the type AST. Three options:

- (a) **Extend `Type.F`** with `ImplicitArrow a a`. Pros: self-describing,
  every reference site sees implicit-ness directly. Cons: changes
  type-hashing — every existing type with `=>` (none yet, but every type
  added going forward) hashes differently than its `->` cousin would.
- (b) **Side-table on declarations.** `Type.F` keeps only `Arrow`; a map
  from `Reference` to "positions of this declaration's arrows that are
  implicit" lives alongside. Pros: preserves type-hash compatibility.
  Cons: every reference site must consult the side-table to know what to
  elaborate.
- (c) **Wrapper type `Implicit a`.** Surface syntax leaks; rejected.

Recommended starting point: (a) with the type-hash break documented as a
v1 consequence (acceptable since `=>` is new syntax and creates only new
types). Final choice in ADR 019.

**Exit:** `Type.F` modified per ADR 019; type-hashing tests updated;
parser+typechecker can synthesize and check the new arrow form.

### 5.4 Subproject 2.C.2 — Typechecker integration
- Recognize `=>` in signatures: desugar (or directly type-check, per
  2.C.1) to function with leading parameters tagged `Implicit`.
- Recognize `given` declarations: validate body's type matches conclusion;
  flag in namespace metadata.
- During inference, record `Implicit` parameter slots as constraint goals
  with types pinned by surrounding inference.
- Do not resolve them yet — that's 2.D.
- New typechecker output: `[(SourceLocation, Type, ScopeSnapshot)]` of
  constraint goals.
- **Lexical given environment.** Thread a `Map Hash Type` (or similar) of
  in-scope local givens through every binding form in
  `Unison.Typechecker.Context` — `Lam`, `Let`, `LetRec`, `Match`-arm,
  top-level decl. This is mechanical but pervasive (the file is ~3800
  lines). Plan for it explicitly; do not undercount.
- **Loop ordering vs TDNR.** Decide whether implicit resolution runs as a
  post-pass after TDNR's fixed point, or interleaved within TDNR's loop
  (per ADR 012). One-shot post-pass is the cheaper starting point and
  likely suffices.

**Exit:** typechecker accepts all examples in the RFC's "consuming givens"
section, producing constraint-goal lists. Lexical given environment is
threaded through every binder. TDNR test suite passes unchanged.

### 5.5 Subproject 2.D — Elaborator pass
- Lift the spike resolver into the real codebase. Reference
  implementation: `spike/implicits/`.
- Input: typechecker output + namespace given-set.
- Output: term with implicit arguments filled in, constraint goals erased.
- Specificity (per refined ADR 008: one-way matching of declared
  conclusions, B's variables flexible, A's rigid), lexical-inner-wins,
  cycle detection, depth limit, memoization (per ADR 023: per-resolve,
  zonked-goal key, cache successes and failures, invalidate on metavar
  binding in the goal stack).
- **Four** structured error categories: `NoGiven`, `Ambiguous`,
  `DepthExceeded`, `Cycle [Ty]`. The spike used `NoGiven` as a stand-in
  for cycle hits but this conflates "no instance" with "instance exists
  but cyclic". Distinguish for diagnostics.
- `NearMiss` carries the unifying substitution alongside the candidate
  given, so error messages can say "tried `Show.list` with `a := Nat`,
  failed because: …" instead of just naming the candidate.
- **Output mechanism mirrors TDNR.** Today TDNR records its decisions as
  `SolvedBlank` info notes that `applyTdnrDecisions` walks and substitutes
  (`FileParsers.hs`). Implicit resolution should produce analogous
  decision records — "fill this implicit hole with this term" — and a
  post-pass walks the term to substitute. Reuses the existing pattern.

**Subgate:**
- 50+ positive cases (chaining, HKT, locals, overrides).
- 30+ negative cases (cycles, ambiguity, depth, missing).
- Property test: elaborated terms typecheck as ordinary terms (round-trip
  through inference unchanged).
- **Property test: cycle detection with metavars.** The spike's cycle
  hits use `unify` rather than `==`, which can spuriously fire when an
  outer goal carries unresolved metavars. Cover this with a property
  test using an inference state that mirrors what TDNR will produce.
- **Property test: diamond resolution with shared metavars.** The spike
  validated diamonds with ground goals only. Phase 2.D goals routinely
  have un-pinned metavars; ensure memoization invalidation (ADR 023) is
  correct when two diamond paths share a metavar that one of them binds.

**Exit:** existing test suite passes (zero regressions). New tests pass.

### 5.6 Subproject 2.E — Hashing and update semantics
- Confirm: elaborated term hashes against resolved dictionary hashes.
- Confirm: given declaration hashes against its body; givenness not in
  *term* hash (but does change *branch* hash, per §1.1 principle 4).
- `update` with re-elaborated term: failures comprehensible.
- `update` that changes resolution silently: new term has new hash, old
  term retains its old hash and old resolution.

**Exit:** update tests show: (a) working update through a given changes
hash; (b) breaking a given leaves dependent terms valid (hashed against
the old) but breaks new code; (c) introducing a second matching given
causes new code to fail with ambiguity but doesn't disturb existing code.

### 5.7 Subproject 2.F — LSP integration

Not previously planned; surfaced by codebase review.

`unison-cli/src/Unison/LSP/` consumes typechecker info notes
(`VarBinding`, `VarMention`, etc.) to produce hover, goto-definition,
document-symbols, and diagnostics. Implicit-resolution synthesizes
arguments that didn't appear in source, so the existing flows have
undefined behavior on them.

- Hover on a synthesized `@d` argument: show "implicit; resolved from
  given `<name>` (#hash)". Source-position hover skips synthesized args.
- Goto-definition through an implicit: jumps to the resolved given.
- Document-symbols: givens appear with a distinct icon/marker.
- Diagnostics: NoGiven/Ambiguous/DepthExceeded errors surface as LSP
  diagnostics with code-actions ("define given `Show Nat`", "shadow with
  local given").

**Exit:** the four LSP flows have a defined, tested behavior on terms
containing resolved implicit arguments. Editor integration tested in
VSCode and at least one other editor.

### 5.8 Phase 2 gate
- 2.A, 2.B, 2.C.1, 2.C.2, 2.D, 2.E, 2.F all green.
- Full test suite green on a branch with the feature flag enabled.
- Performance: typechecking unison-base codebase no slower than ±5% with
  flag on (no givens used yet).

---

## 6. Phase 3 — UCM integration

- `view <name>` on a given shows the `given` keyword.
- `find` learns a `:given` filter.
- `find <type>` notices when type is a constraint and surfaces matching
  givens specially.
- `edit` on a given works exactly like editing any definition; re-saving
  preserves the given tag.
- New command `givens` lists givens in current namespace.
- New commands `mark.given <name>` and `unmark.given <name>` to toggle the
  namespace tag without rehashing the term (the branch hash will change).
- `move`, `alias`, `delete`, `fork` updated per ADR 013.
- `pull` and `push` carry the given-set.

**Exit:** every UCM command's help text and behavior updated. Manual smoke
test against a real project. CI tests cover new commands.

---

## 7. Phase 4 — Pretty-printer and errors

### 7.1 Printer
- Default: elide implicit arguments; render `=>` in signatures; render
  `given` on declarations.
- Verbose mode: print resolved implicit arguments inline as `@<dict>`.
- Round-trip identity in both modes.
- **Round-trip property test:** for any well-typed term `t`,
  `parse(print(elaborate(t))) == elaborate(t)` in both verbose and elide
  modes, including local givens and explicit `@`-overrides. Elide mode
  must produce source that re-elaborates to the same term (same chosen
  givens), since the printed source no longer carries the explicit
  arguments.

### 7.2 Errors

Structured output for the existing error-rendering machinery:

- **NoGiven**: requested type, scope summary, near-misses, hint.
- **Ambiguous**: candidates with namespace path and hash prefix, hint.
- **DepthExceeded**: print chain (truncated sensibly), mark recursive
  step, hint.

Run errors through real users before signing off.

**Exit:** golden-file tests for 20+ distinct error scenarios. Recorded
session showing realistic debug flow under each error category.

---

## 8. Phase 5 — Stdlib seed

- `Eq`, `Ord` for primitives + collections.
- `Show` for primitives + collections.
- `Functor`, `Applicative`, `Monad` for `Optional`, `Either`, `List`,
  common containers.
- `Traversable` for the same.
- One *non-trivial* given showing chaining: `Show (Map k v)` requiring
  `Show k` and `Show v`.
- One example program in the docs using 3+ givens together.

**Exit:** stdlib givens reviewed and accepted on their own merits. Example
program in tutorial form.

---

## 9. Phase 6 — Release

- Feature flag *off by default* through one release cycle for library
  authors to experiment.
- Documentation: language tour section, reference docs, ADR index linked
  from the docs.
- Migration guide: how to add givens to existing libraries; how to *not*.
- Flip default to *on* in the following release.

**Exit:** flag flipped, no rollback for one full release cycle, no
critical bugs filed.

---

## 10. Risk register

| Risk                                                       | Mitigation                                                  |
|------------------------------------------------------------|-------------------------------------------------------------|
| Resolution algorithm too slow on large codebases           | Spike microbenchmark + Phase 2 perf gate                    |
| Errors are inscrutable (Haskell-failure mode)              | Phase 4 dedicated; user-tested before sign-off              |
| HKT inference interacts badly with TDNR                    | Spike covers HKT; Phase 2.C explicit HKT tests              |
| Update semantics surprise users                            | ADR 016 + Phase 2.E explicit testing                        |
| Ecosystem fragmentation (every library defines its `Show`) | Phase 5 stdlib givens establish canonical types early       |
| Hash drift: elaborator changes silently re-hash code       | Elaborator output deterministic; lock with golden-hash tests |
| Feature creep (defaults, conversions, deriving)            | ADR set explicitly defers; reject in review                 |
| TDNR and given-resolution disagree at the same call site   | ADR 012 defines precedence; test matrix covers both kinds at one site |
| Keyword migration breaks existing code (`given`, `summon`) | ADR 022; deprecation cycle or use prefix sigils as fallback |
| Sharing API protocol drift between client and server       | ADR 020 versions the protocol; coordinated server release in Phase 2.B |
| `Unison.Typechecker.Context` (~3800 LoC) bloats further    | Threading lexical given env is pervasive — budget Phase 2.C.2 accordingly; refactor incrementally |
| `unison-merge` lacks given-set conflict story              | ADR 021 + Phase 2.B explicit subtask                        |

---

## 11. Deliberate omissions

- A timeline. Effort estimates require knowing capacity.
- A "deriving" mechanism. Belongs in v2.
- A migration of TDNR-as-typeclass-illusion code. The illusion keeps
  working; users opt into givens.

---

## 11A. Open questions surfaced during ADR authoring

Each item below is called out in a specific ADR's "Open question" or
"Consequences" section. They aren't blocking the spike, but should be
resolved before the corresponding implementation phase.

1. **ADR-019 multi-constraint encoding.** Should `(C1 a, C2 b) =>` desugar
   to nested single-implicit arrows or to a multi-arg implicit
   constructor? Affects pattern-matching ergonomics across every `Type.F`
   consumer. *Settle before Phase 2.C.1.*
2. **ADR-020 fate of `causal_metadata`.** Recommended to leave dormant
   rather than drop. Documentation/cleanup obligation; not currently in
   the risk register.
3. **ADR-021 merge prompt UX in CI.** Case (b) "mark wins,
   user-confirmable" needs an explicit policy for non-interactive runs.
   Current recommendation: default Y in CI. *Pin before Phase 2.B.*
4. **ADR-021 `unison-merge` type shape.** Whether existing types need new
   metadata-diff variants or a parallel structure suffices. Starting
   recommendation: parallel structure for cheaper landing; revisit after
   spike.
5. **ADR-022 proactive reservation of `using`.** Even though ADR-001
   chose `=>`, future ADRs may want it. Track so a future syntax addition
   isn't blocked by another deprecation cycle.
6. **ADR-018 eventual removal of off-state.** Once the flag flips to
   default-on, the codebase carries two parser modes indefinitely. Track
   when (or if) the off-state retires.
7. **ADR-015 / ADR-016 round-trip precondition.** Elide mode plus a
   namespace whose given-set has shifted produces the deterministic
   failure path of ADR-016. The §7.1 round-trip property assumes "same
   namespace" — make that precondition explicit.
8. **ADR-017 `delete` on aliased hash with one-name-given-tagged.** ADR
   specifies per-name tag (deletion only removes the local tag); this can
   contradict a "givenness is per-hash" reader expectation. Worth a docs
   note in Phase 3.

---

## 12. Codebase reference index

Critical files identified during the codebase-validation review. Use as a
navigation map for implementers; line numbers are accurate at the
review's snapshot but will drift.

### TDNR and the typechecker
- `parser-typechecker/src/Unison/Typechecker.hs` — `typeDirectedNameResolution`
  (~line 262) drives the TDNR fixed-point loop. Implicit resolution will
  be a sibling pass with the same `SolvedBlank`/`Decision` shape.
- `parser-typechecker/src/Unison/FileParsers.hs` — `applyTdnrDecisions`
  (~line 329) walks the term and substitutes TDNR results. Implicit
  substitution can mirror this.
- `parser-typechecker/src/Unison/Typechecker/Context.hs` — ~3800 LoC of
  bidirectional inference. The lexical given environment must be threaded
  through every binder here. `InfoNote` (~line 388) is where new
  constraint-goal records will live. `Element` (~line 131) defines
  context shape; resist adding new forms unless required.
- `parser-typechecker/src/Unison/KindInference/` — full kind inference
  pipeline (`Generate`, `Solve`, `Constraint`, `Error`, `UVar`). Already
  supports arrow kinds (`Type | Ability | Kind :-> Kind`); HKT givens
  work without extension.

### Term and type AST
- `unison-core/src/Unison/Term.hs` — ABT `F` functor (~line 64). `App` is
  generic; elaborator output uses ordinary `App` nodes.
- `unison-core/src/Unison/Type.hs` — `Type.F` (~line 40). ADR 019 picks
  whether to extend with `ImplicitArrow` (changes type hashing) or use a
  side-table.
- `unison-core/src/Unison/Blank.hs` — `Recorded` (~line 21) is the
  parser-emitted hole. Add an `Implicit` variant.

### Codebase storage and metadata
- `codebase2/codebase/U/Codebase/Branch/Type.hs` — `MdValues = Set
  MetadataValue` (~line 32) per `(NameSegment, Referent)`. Likely home
  for the given tag; ADR 020 confirms.
- `codebase2/codebase-sqlite/sql/create.sql` — schema. Existing
  `causal_metadata` table (~line 113) is dormant; ADR 020 decides
  revival vs. alternative.
- `unison-merge/src/` — currently no metadata-aware logic. ADR 021 +
  Phase 2.B add given-set conflict resolution here.

### Hashing
- `unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs` — branch hash
  derivation (~line 18). Includes `MdValues` in tokens, so givenness
  changes the branch hash even though term hashes are stable.

### Lexer and parser
- `unison-syntax/src/Unison/Syntax/ReservedWords.hs` — reserved word
  list. `given`, `summon`, `using`, `=>` not yet taken.
- `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` — the lexer.
  - `?` delimiter at ~line 489 (Char literal prefix; explains the
    rejected `?T` syntax).
  - `@` symboly keyword at ~line 572 (currently used for as-patterns and
    `@rewrite`; expression-context `@` for explicit override needs
    parser disambiguation).
  - Order of `==>` vs `=>` in keyword alternatives matters
    (`symbolyKw` at ~line 586).
- `parser-typechecker/src/Unison/Syntax/TermParser.hs` — `@`-pattern
  parsing (~line 410). Confirm coexistence with new expression-context
  `@`.
- `parser-typechecker/src/Unison/Syntax/Parser/Doc.hs` — `@`-prefixed
  doc syntax (~line 163). Another disambiguation point.

### UCM and editor integration
- `unison-cli/src/Unison/CommandLine/InputPatterns.hs` — flat table of
  commands (~4671 lines). Add `givens`, `mark.given`, `unmark.given`;
  modify existing commands as needed.
- `unison-cli/src/Unison/Codebase/Editor/HandleInput/` — actual command
  implementations; metadata plumbing lands here.
- `unison-cli/src/Unison/LSP/` — language-server flows; Phase 2.F.

### Sharing
- `unison-share-api/` and `unison-share-projects-api/` — wire format
  versioning when metadata field is added.

### Test corpora
- `unison-src/transcripts/idempotent/higher-rank.md` — confirms HKT
  works in current Unison (`unique type Functor f` + polymorphic use).
  Use as a baseline for HKT given tests.
