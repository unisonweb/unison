# Phase 2 chunk catalog

Each chunk is sized for ~1 week of focused work for a normal developer
familiar with the relevant Unison subsystem. Each chunk has:

- A clear deliverable (code + tests)
- An explicit dependency list
- An assigned implementer agent and reviewer agent (different agents)
- A worktree branch that the reviewer reads

Status legend: ⏳ queued · 🟡 implementing · 🟠 in review · ✅ done · ❌ blocked

## Chunk dependency graph

```
A1 ──┐                     (parser: constraint syntax)
A2 ──┤                     (parser: given decls + summon + let-given)
A3 ──┘── A4                (parser: @-override · A4 = parser snapshot suite)

B1 ──┬── B2                (UCM mark/unmark commands)
     ├── B3                (sharing API + push/pull)
     └── B4                (unison-merge conflict resolution)

C1.1                       (Type.F ImplicitArrow extension — foundational)
   └── C2.1                (thread lexical given env through Context.hs)
        └── C2.2           (constraint-goal emission from inference)
             └── C2.3      (given-decl validation)
                  └── D1   (port spike resolver into parser-typechecker)
                       └── D2  (adapt to real Type/unification)
                            └── D3 (applyTdnrDecisions-shape post-pass)
                                 └── D4 (errors + property tests)
                                      ├── E1 (update + hashing tests)
                                      ├── F1 (LSP hover + goto-def)
                                      └── F2 (LSP diagnostics + code-actions)
```

## Wave plan

**Wave 1 (parallel):** A1, B1, C1.1
**Wave 2 (after Wave 1):** A2, B2, B3, B4, C2.1
**Wave 3 (after Wave 2):** A3, C2.2
**Wave 4 (after Wave 3):** A4, C2.3
**Wave 5:** D1
**Wave 6:** D2
**Wave 7:** D3
**Wave 8:** D4
**Wave 9 (parallel):** E1, F1, F2

Reviewer for each chunk runs immediately after that chunk's implementer.

## Chunk specifications

### A1 — Parse `=>` constraint syntax in type signatures

**Deliverable:** lexer recognises `=>`; type parser accepts `(C1 a, C2 b) => T -> U` form; AST extends `Type.F` parsing path with constraint-bearing arrows. Snapshot tests for round-trip.
**Files (estimated):** `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs`, `unison-syntax/src/Unison/Syntax/ReservedWords.hs`, `parser-typechecker/src/Unison/Syntax/TypeParser.hs`.
**Out of scope:** typechecker semantics (downstream chunks); printer (Phase 4); `given` keyword (chunk A2).
**Dependencies:** none. Foundational. Use ADR-019's recommended `ImplicitArrow` shape but the AST hookup happens in C1.1; until then, A1 emits the closest existing form and tags the source range so C1.1 can rewire it.
**Acceptance:** parser round-trips all signature forms in `docs/implicits-plan.md` §1.2; snapshot tests committed under `parser-typechecker/tests/`.

### A2 — Parse `given` declarations, `summon T`, `let given`

**Deliverable:** top-level `given` defs, `let given x = ...` blocks, `summon T` expressions, all parse to AST nodes that downstream phases can ignore (no semantics yet).
**Files:** `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` (add keywords), `Syntax/{Term,File,Block}Parser.hs`.
**Dependencies:** A1 must merge first to avoid lexer-edit conflicts.
**Acceptance:** all three forms parse; snapshot tests. Keyword migration: hard-break is acceptable for the prototype (existing identifiers `given`/`summon` lex as `Reserved`, parse-failing). ADR-022's deprecation-cycle is a release-engineering concern that lands separately; it is *not* required for the RFC prototype. Document the choice in a code comment near the keyword promotion.

### A3 — Parse `@`-positional explicit override

**Deliverable:** `f @ d x` parses as explicit-implicit-override application; coexists with `@` in pattern (`Foo@Bar`) and doc (`@rewrite`) contexts.
**Files:** `Syntax/TermParser.hs`, possibly `Syntax/Parser/Doc.hs`.
**Dependencies:** A2 must merge first (lexer state).
**Acceptance:** snapshot tests for override + as-pattern + doc `@` show no regression.

### A4 — Parser snapshot suite consolidation

**Deliverable:** consolidated test module covering A1+A2+A3, including parse-error scenarios for malformed `=>`, missing `given` body, etc.
**Files:** new test module under `parser-typechecker/tests/`.
**Dependencies:** A3.
**Acceptance:** ≥30 snapshot tests; parse errors are helpful.

**Carry-overs from A1 review (nice-to-haves, not blockers):**
- Tighten the TODO comment in `TypeParser.hs:42-49` to say "C1.1 will replace `Type.arrow` with `Type.implicitArrow` at the call site" rather than the more abstract "post-pass" framing.
- Add a positive test for `Show a => Show (List a)` (given-conclusion form, §1.2).
- Add a positive test combining constraints with effect arrows: `(Monad m) => (a ->{e} m b) -> ...` (§1.2).
- Add a negative test pinning that nested `=>` chains (`Show a => Eq a => T`) are rejected per ADR-001.

**Carry-overs from A2 review:**
- Add four negative tests to `ImplicitParser.hs`: (a) `given` with no body, (b) `summon` with no argument, (c) top-level `given x = …` with no `:` annotation, (d) `let given x = …` with no annotation.
- Add one positive test exercising A1 ⊕ A2: `given x : Foo a => Bar a = …`.
- Add lexer-level tests in `unison-syntax/test/Unison/Test/Unison.hs` pinning that `given` and `summon` lex as `Reserved`.
- Fix the `label` helper in `ImplicitParser.hs` so multi-line `let-given` fixtures get distinct scope names (currently both display as `let-given.let`).
- (Nice to have) Drop `P.try givenBinding` in `TermParser.hs:1096` and commit after the `given` keyword — current `P.try` produces unhelpful errors on malformed `given`.
- (Nice to have) Tighten `summonExpr`'s docstring at `TermParser.hs:696-698` to match its actual greedy `valueType` parse.

**Carry-overs from A3 review:**
- Add a structural test that pulls out the second arg of a parsed `f @ d` and asserts the widened annotation actually distinguishes override args from regular args (e.g., `ann.start.column < ann d.start.column`). Otherwise a regression that drops the widening would silently pass.
- Add negative tests for malformed override syntax: (a) `f @` with no following arg, (b) `@d` at term head (no preceding term), (c) `f @ @ x` (double `@`).
- Use `ABT.annotate widened d` instead of record-update at `TermParser.hs:968` (cosmetic).
- Same `label` collision noted in A2 carry-overs applies to the new doc-rewrite test cases.

**Open architectural question (NOT for A4 — must settle before chunk D2):**
- A3 used annotation-widening to mark override-arguments; the plan calls for an "AST node for implicit-application override." Settle: either (a) add a sentinel wrapper in `unison-core` so D2/D3 can match on it, or (b) keep the annotation-widening contract and add property tests proving the widened ann survives parse→print→parse round-trips. Track this as a D-stream prerequisite.

### B1 — Givenness sentinel + `MdValues` plumbing

**Deliverable:** built-in `##Builtin.Given` reference; `MdValues` reads and writes plumbed through `unison-cli/src/Unison/Codebase/Editor/HandleInput/` for `view`/`find`; helper module `Unison.Codebase.Givens` for predicate `isGiven :: Codebase -> Referent -> m Bool`.
**Files:** `parser-typechecker/src/Unison/Builtin.hs`, new `Unison.Codebase.Givens`, edits in `HandleInput/View.hs`, `HandleInput/FindI.hs`.
**Dependencies:** none.
**Acceptance:** unit tests show `MdValues` round-trip; `view` on a given-tagged definition prints `given` keyword (parser side handled by A2 — for now, just the data flow).

### B2 — UCM `mark.given`/`unmark.given` commands

**Deliverable:** new commands in `InputPatterns.hs`; corresponding handlers; `givens` listing command.
**Files:** `unison-cli/src/Unison/CommandLine/InputPatterns.hs`, `unison-cli/src/Unison/Codebase/Editor/HandleInput/`.
**Dependencies:** B1.
**Acceptance:** transcript tests under `unison-src/transcripts/` mark, list, unmark.

**Carry-overs from B1 review (close as part of B2):**
- Wire `Unison.Codebase.Givens.isGiven` into the `view` rendering path in `unison-cli/src/Unison/Codebase/Editor/HandleInput/ShowDefinition.hs` — surface the `given` keyword on definitions that have the sentinel. (Parser-side `given` keyword is from chunk A2; together this closes the round-trip.)
- Wire `Unison.Codebase.Givens.metadataValuesFor` into the `find` filtering path in `HandleInput/FindAndReplace.hs` — let users filter for given-tagged definitions.
- Reconcile the contradictory docstrings in `Givens.hs:15-16` vs `:60-66` — drop the "per-name granularity" bullet.
- Strengthen the misnamed "term reference" test in `Unison.Test.Codebase.Givens` (parser-typechecker tests) to actually compute and compare term-hash before/after marking, validating ADR-014 empirically.

### B3 — Sharing API push/pull verification

**Deliverable:** verification that `MdValues` (and therefore the given sentinel) travels through push/pull intact via the existing wire format. **No protocol version bump** — chunk B3's investigation confirmed the wire format already carries metadata references opaquely (see amended ADR-020). Future feature gating uses the future-compatible `StreamInitInfo` map at Phase 3.
**Files:** `unison-share-api/tests/Unison/Test/Sync/GivenRoundtrip.hs` (new).
**Dependencies:** B1.
**Acceptance:** new round-trip test confirms the sentinel survives the wire format; commit body documents why no bump is needed.

### B4 — `unison-merge` given-set conflict resolution

**Deliverable:** the four conflict cases from ADR-021 (both-mark, one-mark, rename-different, mark-vs-delete).
**Files:** `unison-merge/src/`.
**Dependencies:** B1.
**Acceptance:** ≥4 unit tests covering each ADR-021 case (transcript tests deferred — see B4 carry-overs).

**Carry-overs from B4 review:**
- Add a TODO marker in `unison-merge/src/Unison/Merge/Mergeblob.hs` (or in the haddock of `GivenSet.hs`) pointing to where the merge engine will call `mergeGivenSets`/`applyGivenSet`. The module is currently sidecar — pipeline integration is a follow-up chunk.
- Document alias-collapse behavior in `applyGivenSet` haddock, OR change `survivorByRef` to `Map Referent (NonEmpty Name)` so multiple surviving names of the same referent are preserved. Two-line haddock is sufficient if the engine doesn't yet exercise aliases.
- Transcript tests covering the four cases through actual merges (vs. unit tests on `GivenSet`) — defer to a follow-up chunk after B4's pipeline integration lands.

### C1.1 — Extend `Type.F` with `ImplicitArrow`

**Deliverable:** `Type.F` gains `ImplicitArrow a a` per ADR-019; hashing-v2 tokens updated; all construction/destruction sites updated; existing `Arrow` cases unchanged in behavior.
**Files:** `unison-core/src/Unison/Type.hs` (the central one), `unison-hashing-v2/src/Unison/Hashing/V2/Type.hs`, every `case` over `Type.F` across `parser-typechecker/`, `unison-runtime/` (where it just delegates to `Arrow`), `unison-cli/`.
**Dependencies:** none. **Foundational** for C2.x and D1.
**Acceptance:** existing test suite zero regressions; new tests confirm `ImplicitArrow` hashes distinctly from `Arrow`; round-trip serialization preserved.

### C2.1 — Thread lexical given env through `Context.hs`

**Deliverable:** new `Map TermReference Type` field on `Unison.Typechecker.Context.Context`; updated through `Lam`, `Let`, `LetRec`, `Match`-arm, top-level decl. No semantics — just plumbing.
**Files:** `parser-typechecker/src/Unison/Typechecker/Context.hs`.
**Dependencies:** none architecturally, but easier after C1.1 lands.
**Acceptance:** zero regressions on existing typechecker test suite; ad-hoc test confirms env propagates through nested binders.

**Implementation note (recorded post-landing):** the field landed on `Env` (the typechecker's monad state), not on `Context` (the `newtype` over a finger-tree of `Element`s). Save/restore via `withLexicalGivens` mirrors the `markThenRetract` pattern. Trade-off: avoids touching the `Measured`/finger-tree machinery at the cost of requiring binder additions to remember the wrapper. Helpers `getLexicalGivens` / `extendLexicalGiven` / `withLexicalGivens` are exported for C2.2/C2.3.

**Carry-overs from C1.1 review (close before this chunk lands):**
- Add `Type.ImplicitArrow' i o -> wellformedType c i && wellformedType c o` clause in `Context.hs:748` to keep `wellformedType` total — currently falls through to a `Match failure` `error` for `ImplicitArrow`.
- Add `ImplicitArrow'` arms to `unArrows` and `unEffectfulArrows` in `unison-core/src/Unison/Type.hs:223–240`. These are consumed by `DeclPrinter`, `TypePrinter`, `Variance.split`, `Context.checkWanted`'s argument-extractor; missing arms silently break declaration-field counting and printing.
- Add `ImplicitArrow'` traversal to `existentializeArrows` and `purifyArrows` (`unison-core/src/Unison/Type.hs:706-735`) so effect-attach/strip behavior matches `Arrow`.
- Add `-- TODO Phase 4: render ImplicitArrow as =>` comment at `TypePrinter.hs:127` and `PrintError.hs:1438` so the printer gap is grep-able. (Don't implement the rendering — Phase 4.)
- Consider adding `AppImplicitArrow` constructor to `KindInference.Generate.hs` `Provenance` so kind errors point at `=>`. (Nice to have; defer if scope-creep.)

### C2.2 — Emit constraint goals from inference

**Deliverable:** new `InfoNote` variant `ConstraintGoal { loc, type, scope }`; inference emits one for each `ImplicitArrow` parameter at apply sites; `Blank.Recorded` gains `Implicit loc` variant.
**Files:** `unison-core/src/Unison/Blank.hs`, `parser-typechecker/src/Unison/Typechecker/Context.hs`.
**Dependencies:** C1.1, C2.1.
**Acceptance:** typechecker accepts test programs with `=>` signatures, emitting the right number of constraint goals at the right locations. No resolution yet — goals are reported, not filled.

**Carry-over from C2.1 review:** strengthen the `lexicalGivensThreading` test (now placeholder per C2.1) so it actually verifies env propagation: e.g., assert `getLexicalGivens` returns a populated map inside a binder body. The C2.2 hook for emitting `ConstraintGoal` notes captures the env, so this test gains a real observation point.

**Carry-overs from C2.2 review (close in next C-stream chunk or a polish pass):**
- Add a multi-constraint emission test: `(C1 a, C2 b) =>` should emit exactly 2 goals in order. C2.2's recursion handles this implicitly but no test pins the property.
- Replace the test's `mapMaybe'` reimplementation with `Data.Maybe.mapMaybe`.
- Add `-- TODO chunk D1+` comment markers at `Context.hs:3009` (`subtype`) and `Context.hs:3121` (`equate0`) — neither has an `ImplicitArrow'` clause; not needed for C2.2 (apply-sites peel off ImplicitArrow first) but D1+ should close.

**C1.1 carry-over actually closed in C2.2 (recorded for posterity):**
- `Context/Structure.hs apply'` totality fix (added `ImplicitArrow'` clause). This was a latent gap from the original C1.1 carry-over commit, surfaced when `ConstraintGoal` types started flowing through `substituteSolved`.

### C2.3 — Validate `given` declarations

**Deliverable:** when typechecker encounters a `given` declaration, validates body's type matches conclusion of declared signature. Hooks into `mark.given` namespace tagging from B1.
**Files:** `parser-typechecker/src/Unison/FileParsers.hs`, `Typechecker.hs`.
**Dependencies:** C2.2, B1.
**Acceptance:** valid given decls pass; mismatched ones produce helpful errors.

**Carry-overs from C2.3 review (nice-to-haves, defer to a polish pass):**
- `unImplicitArrows` haddock at `unison-core/src/Unison/Type.hs:266-272` mentions a `vs` list that the implementation doesn't return; tighten the doc to match the actual `([Type v a], Type v a)` shape.
- `noteGivenDeclLet` (`Context.hs:1292`) takes `_inferredType` that's never used; drop it or note why it's there (callsite-symmetry with `noteTopLevelType`).
- `emitGivenDeclNote` and `noteGivenDeclLet` are nearly identical with different binding-shape destructuring; can be unified via a shared helper.
- Add a `-- TODO chunk B2/D3:` marker at `givenDeclVars`'s call-out site (currently no in-tree consumer; downstream chunks consume it).

### D1 — Port spike resolver into `parser-typechecker`

**Deliverable:** new module `Unison.Typechecker.GivenResolver` containing the spike's algorithm, adapted to use `Unison.Type.Type v loc` and the existing unification primitives in `Context.hs`. Does NOT yet run end-to-end — exercised via unit tests.
**Files:** new `parser-typechecker/src/Unison/Typechecker/GivenResolver.hs` and unit tests.
**Dependencies:** C1.1.
**Acceptance:** unit-tested resolver passes the spike's 15-test matrix on real `Unison.Type.Type` values.

### D2 — Adapt resolver to real unification context

**Deliverable:** resolver consumes the constraint goals emitted by C2.2 and the lexical given env from C2.1; produces resolution decisions analogous to TDNR's `SolvedBlank`.
**Files:** `Unison.Typechecker.GivenResolver`, `Typechecker.hs`.
**Dependencies:** C2.2, D1.
**Acceptance:** integration tests on small files with `=>` signatures produce correct resolutions.

**Carry-over from D1 review:**
- `cycleHit` in D1 uses `unify mempty Set.empty` (no flex) — equivalent to structural equality up to annotations, not the alpha-renamed-cycles semantics the docstring claims. In D1 with closed monotypes this is unobservable; D2 will see metavars and the discrepancy will surface. Either widen flex to all free vars of either side, or rephrase the docstring honestly.
- D2 must decide on shared-metavar awareness for both unification and memoization (per ADR-023's metavar-invalidation rule), and may want to revisit whether to fold the resolver into the `M v loc` monad at that point or keep it standalone.

### D3 — Apply resolved givens via post-pass

**Deliverable:** `applyGivenDecisions` analogous to `applyTdnrDecisions`; walks the term and substitutes implicit applications into `App` nodes.
**Files:** `parser-typechecker/src/Unison/FileParsers.hs`, `Typechecker.hs`.
**Dependencies:** D2.
**Acceptance:** end-to-end: file with `=>` signatures and matching givens typechecks and elaborates; the resulting term has plain `App` nodes pointing at dictionary hashes.

**Resolution of A3 architectural question (was: AST wrapper vs annotation-widening for `@`-overrides):** **chosen option (b) — annotation-widening contract.** D3 walks the term and threads `SolvedImplicit` decisions into `App` nodes. When D3 encounters an apply site whose next argument has a widened annotation (per A3's `Term.apps` convention — start column of the arg's range begins before the arg's leaf token, indicating an `@`-prefixed override), D3 must:
1. NOT consume a `SolvedImplicit` decision for that slot.
2. Leave the user-supplied argument in place.
3. Continue walking — the next `App`'s argument may or may not be widened; each is checked independently.

Add a property test: `parse(print(elaborate(t)))` preserves the override marker, since D3 must not strip the widened annotation when threading decisions.

### D4 — Error categories and property tests

**Deliverable:** structured `NoGiven`, `Ambiguous`, `DepthExceeded`, `Cycle` errors; property tests for cycle/metavar interaction and diamond/shared-metavar correctness (see `spike/implicits/FINDINGS.md` and ADR-023).
**Files:** `Unison.Typechecker.GivenResolver`, error-rendering hooks.
**Dependencies:** D3.
**Acceptance:** ≥4 distinct error scenarios with golden-file output; property tests pass.

**Carry-overs from D2 review:**
- Add elaborator-level tests for the `Ambiguous` and `DepthExceeded` categories (D2 only proved `NoGiven` end-to-end). Each should set up a small test program that exercises that specific failure category through the full elaborator path.
- Add a cycle/metavar regression test: a mutually-recursive given pair where one resolution path induces a metavar in the goal. Should exercise the now-fixed `cycleHit` widened-flex behavior.
- When a goal's existential is unsolved at end of inference, the resolver currently treats it as rigid and can produce a "no given" failure where "type can't be inferred" would be more accurate. D4 should detect this and produce a clearer error category (perhaps a fifth variant `UnresolvedMetavarInGoal` or special-case rendering of `NoGiven`).

**Carry-overs from D3 review:**
- `GivenApply.aeLocalTypes` is never extended for `Abs`-introduced bindings; either thread the env through `Abs` in `rewrite` or drop the "traversal-introduced bindings" wording in the doc.
- `interleave` handles bare `f` (no args) at an `ImplicitArrow'` slot by NOT inserting the dictionary (`GivenApply.hs:288-289`). For partial applications like `let g = f in g 42` this silently drops the implicit. Add a regression test and either insert the dict at the partial-app site or document the constraint.
- Effect arrows on the spine break the walker (`GivenApply.hs:313`). `Effect1' es (Arrow' i o)` falls into the catch-all. Functionally OK at the moment (no implicits after an effect arrow at the spine in well-typed surface types) but the walker should peek through `Effect1'` to keep the apply chain coherent.
- Decision queue (`collectDecisions` in `GivenApply.hs:167`) is order-dependent on synthesis traversal order; if the C-stream's traversal order ever changes, this passes silently with wrong dictionaries. Switch to a `loc`-keyed `Map` for hardening.
- Surface "wrap leaf overrides in parens" guidance from D3's `GivenApply.hs:82-85` haddock into a user-facing diagnostic when an unparenthesized `@`-leaf override is silently ignored.
- Cosmetic: `outerAnn` is reused for every dictionary's `Term.ref` annotation, collapsing nested-dict ranges. Consider deriving from the function's annotation per layer.

**Carry-overs from D4 review (deferred polish):**
- `Cycle` constructor at `GivenResolver.hs:163-170` is documented as emitted "for self-referential goals with no escape hatch" but no construction site exists. Either implement the explicit hard-cycle detection, or update the docstring to "Reserved for future use; not currently emitted."
- Thread `candSubst` into `NearMiss` so the renderer can show "tried `Show.list` with `a := Nat`" — the substitution is computed in the resolver but not surfaced.
- Diamond property test docstring at `GivenResolverProperties.hs:191-263` claims to exercise ADR-023's metavar-invalidation rule but the goal uses a ground `a`. Either rename the test or build a goal where `a` is a genuine existential bound during resolution.

### E1 — Update + hashing semantics tests

**Deliverable:** transcript tests for ADR-016's three cases — (a) working update through given changes hash; (b) breaking given leaves dependent terms valid but breaks new code; (c) new ambiguous given fails new code without disturbing old.
**Files:** `unison-src/transcripts/`.
**Dependencies:** D4.
**Acceptance:** all three cases produce expected transcripts.

### F1 — LSP hover and goto-definition on synthesized args

**Deliverable:** hover on `@d` shows "implicit; resolved from given <name> (#hash)"; goto-def jumps to the resolved given.
**Files:** `unison-cli/src/Unison/LSP/`.
**Dependencies:** D4.
**Acceptance:** manual test in VSCode + scripted protocol-level test.

### F2 — LSP diagnostics and code actions

**Deliverable:** NoGiven/Ambiguous/DepthExceeded surface as diagnostics; code actions for "define given X" and "shadow with local given".
**Files:** `unison-cli/src/Unison/LSP/`.
**Dependencies:** D4.
**Acceptance:** scripted protocol-level test for each diagnostic.
