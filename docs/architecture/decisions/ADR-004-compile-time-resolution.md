# ADR-004: Resolution is purely compile-time; runtime/codegen untouched

**Status:** Accepted
**Date:** 2026-05-01
**Deciders:** core maintainers (per design discussion)
**Related:** ADR-002, ADR-014, ADR-019; `docs/implicits-plan.md` §1.1 principle 1, §1.1 principle 2

## Context

Implicit-parameter systems can run at one of two levels. Most ML-family
languages (Haskell, Scala, Idris) elaborate implicits away at compile
time, leaving ordinary function applications behind. A few systems
(notably some dynamic languages and dependently-typed proof assistants
with `auto`-tactics at runtime) defer instance lookup. Choosing affects
which subsystems learn a new concept.

Unison's runtime is content-addressed and built around term hashes that
are baked into compiled output (ANF/MCode in `unison-runtime/`) and
into the codebase store. Anything that has to vary across runtime
contexts breaks the "the term hash *is* the program" model. Conversely,
if elaborator output is plain `App` nodes referencing fully resolved
dictionary hashes, the runtime sees nothing new at all.

## Options considered

### Option A: Compile-time-only resolution

The elaborator runs after typechecking and before any runtime concern.
It produces ordinary `App` nodes whose argument positions are filled
with references to chosen given dictionaries. Term hashing
(`unison-hashing-v2`) hashes those `App` nodes the same way it hashes
hand-written applications. Runtime ANF/MCode in `unison-runtime/`
sees only ordinary applications. Pros: zero changes to runtime,
codegen, ABT, and term hashing; the feature is entirely "elaborator
sugar"; resolution is frozen into the term hash, so a stored term
will never silently re-resolve to a different given when the
namespace changes. Cons: changing a given requires producing a new
term (with a new hash) — ADR-016 has to define `update` semantics for
this.

### Option B: Runtime dictionary lookup

Compiled terms reference givens by name or by some indirect handle;
the runtime looks up the current dictionary at call time. Pros: a
single term can dispatch differently depending on namespace context;
allows late-binding of instances. Cons: the runtime
(`unison-runtime/` ANF and MCode) gains a new instruction class;
term hashes either become context-dependent (defeating
content-addressing) or stop reflecting runtime behavior; the codebase
store has to record dictionary-resolution metadata that travels with
the term; the "two libraries with conflicting `Show.nat` ambiguate at
the call site" coherence story becomes incoherent because the
ambiguity is resolved at the wrong time.

## Decision

We adopt **Option A: resolution is purely compile-time.** The
elaborator emits plain `App` nodes referencing fully resolved
dictionary terms. Runtime, codegen, ABT, and term-hashing learn no
new concept.

This was verified by inspection of the codebase:
`unison-runtime/`'s ANF/MCode pipeline operates on already-elaborated
ABT and never inspects implicit-ness, and
`unison-hashing-v2/src/Unison/Hashing/V2/Branch.hs` hashes branch
contents (not term internals) so it is unaffected by the elaborator
output as long as that output is plain `App` nodes. The whole feature
lives in the elaborator pass.

## Consequences

- `unison-runtime/` is not touched by this feature. ANF, MCode, the
  decompiler, and the runtime FFI all remain unchanged.
- Term hashing is unchanged. An elaborated term with implicit slots
  filled in hashes exactly like the equivalent hand-written term.
  This is a guarantee downstream tooling can rely on.
- Resolution is *frozen into the hash*: once a term is elaborated and
  stored, it references specific dictionary hashes. Adding or
  removing givens later cannot change what an existing stored term
  computes.
- Changing which given a function consumes requires a *new* term (new
  hash). `update` semantics must be defined for the re-elaboration
  case (ADR-016).
- The elaborator pass's output must be deterministic given the same
  typechecker output and the same given-set, or hashes will drift
  across rebuilds. Phase 2.D locks this with golden-hash tests.
- Forecloses the "single term that works in multiple namespaces with
  different instances" use case. Users who want that must abstract
  over the dictionary explicitly (writing the parameter, not relying
  on `=>`).
- Makes tooling unambiguous: a debugger, profiler, or decompiler
  reading runtime values sees the same shape it has always seen.
