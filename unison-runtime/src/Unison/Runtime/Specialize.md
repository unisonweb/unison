# `specialize`

A builtin that specializes a function on its first argument:

```unison
specialize : (a ->{g} b) -> a ->{g} b
```

`specialize f x` means `f x`. It gives the same result, the same effects and the same
termination behaviour. The only difference is speed. When `b` is a function, the result is
new code with `x` folded in: every computation that depends only on `x` has already been
done, and what's left runs on the remaining arguments. Specializing an interpreter on a
program gives compiled code for that program (the first Futamura projection), and the JIT
then makes it native.

It is built as a partial evaluator over the runtime's ANF, running inside the runtime,
whose output is loaded like any other code. The partial evaluator handles only the code
that involves unknown values; everything computed from known values runs on the
interpreter itself.

## The builtin

### Meaning

- **`f` takes two or more arguments.** `f x` is a partial application, so nothing runs.
  `specialize` returns an equivalent closure whose code has `x` folded in.
- **`f` takes one argument.** `f x` runs now, with its effects, exactly as an ordinary call
  would. If the result is a closure, its captured values are all known, so it is
  specialized on them. If the result isn't a closure, it is returned as is.

The core operation is therefore "specialize a closure on everything it has captured":

```
specialize f x = specializeClosure (f x)       -- f x evaluated by the machine as usual
```

`f x` performs whatever effects it performs, and `specializeClosure` performs none. That is
why the type is `{g}` on both sides.

### What a specialized closure is

`specialize f x` means the same as `f x`, but it is allowed to be a different value: it may
compare unequal to `f x` and hash differently. Its identity is "`f` specialized on `x`". A
specialized closure is an ordinary `GPAp` closure over a generated combinator, so:

- **Equality, ordering and hashing** are the existing ones for closures, with no special
  case in the Haskell walkers or the JIT's C helpers. The generated entry's reference is
  derived from a hash of `f` and `x`, not of the generated code. Two specializations of
  the same `f` on the same `x` are therefore equal, in this process and in any other, and
  the identity doesn't change when the specializer improves or its budgets change.
- **What it holds:** the same arguments as the closure it was made from, in the same
  order, followed by any constants the generated code needs. CCache's `specializations`
  maps each generated group to the combinator of the original closure and how many
  arguments that closure had ([`specializeClosure`](Machine.hs)).
- **Serialization** (`Value.value`, remote calls, `Value.serialize`, the crypto hash of a
  value) can't ship the generated code, because no codebase holds it. It ships the
  original closure instead, in a value form of its own:
  - `reflectValue` writes a specialized closure as `ANF.Specialized (Partial orig args)`:
    the original combinator applied to the arguments the specialized closure holds.
    Nested specialized closures work the same way.
  - `reifyValue` turns `Specialized c` back into `specialize` of `c`, which runs the
    specializer. That gives the same identity, because the reference comes from the
    closure.
  - A specialized closure that has been partially applied further holds more arguments.
    It reflects as the original applied to all of them, and reads back as a closure
    specialized on all of them. That closure means the same, but it is a different value.
  - Only version 5 of the value format can carry the form (tag `SpecializedT`,
    [`ValueV5.hs`](ANF/Serialize/ValueV5.hs)). `Value.serialize` writes version 4, which
    older runtimes read, for a value with no specialized closure in it, and version 5 for
    one with
    ([`serializeValueLatest`](ANF/Serialize.hs)). Asked for version 4 explicitly, the
    writer puts the original closure in its place, which means the same, unspecialized.
  - The value hash keeps the tag, so a specialized closure hashes as it compares.
  - A value's dependencies are those of the original closure.
  - The decompiler ([`Decompile.hs`](../../../ucm/Unison/Runtime/Decompile.hs)) shows a
    specialized closure as `specialize f x`, where `f` is the original closure without its
    last argument and `x` is that argument.
- **Stack traces and profiles:** the generated combinator's name says what it was made
  from.

Because the meaning is always `f x`, an implementation that returned `f x` unchanged would
be correct. Every later phase only makes the result faster.

### Mutable values and continuations

`x`, or something inside it or captured by `f`, may be an `MVar`, a `Ref`, a mutable
array, a `TVar`, a handle, or a captured continuation.

- **Mutable values:** which object it is never changes. What it holds can change, so the
  specializer treats it as a known but opaque constant. It is passed along and stored, and
  every operation on it (take, put, read, write) is residualized, because those primops
  are impure. The residual captures the object itself, so the specialized function shares
  it with `f x`, as it must. Nothing is gained, and nothing breaks.
- **Continuations** are immutable: a list of frames, each a known combinator section with
  the values it saved. The first version treats one as an opaque constant too, and a
  resume of it is residualized. Unfolding a known continuation's frames is possible later,
  and it would compile "the rest of the computation" with the saved values folded in.
- **Identity and the memo table:** a closure holding one of these can't be serialized, so
  its contents can't be hashed. It gets a fresh reference instead
  ([`specializedRef`](Machine.hs)): it is equal only to itself, and specializing it again
  generates its code again.
- **When `f x` isn't a closure:** this covers `f` being a continuation, in which case
  `f x` resumes it. `specialize` returns the result of `f x` unchanged.

## Example: the first projection

A small language with numbered top-level functions, each taking one argument:

```unison
type Expr = Num Nat | Arg | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
          | IfZero Expr Expr Expr | Call Nat Expr

eval : [Expr] -> Expr -> Nat -> Nat
eval prog e x = match e with
  Num n        -> n
  Arg          -> x
  Add a b      -> eval prog a x + eval prog b x
  Sub a b      -> eval prog a x - eval prog b x
  Mul a b      -> eval prog a x * eval prog b x
  IfZero c t f -> if eval prog c x == 0 then eval prog t x else eval prog f x
  Call i a     -> eval prog (List.at! i prog) (eval prog a x)

run : [Expr] -> Nat -> Nat
run prog = eval prog (Call 0 (Arg))

factorial = [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))]

fac : Nat -> Nat
fac = specialize run factorial
```

`fac` should come out as the residual of `eval` on the static `prog` and the static body
of function 0, with only `x` dynamic:

```
fac0 x = if x == 0 then 1 else x * fac0 (x - 1)
```

The interpretive overhead is gone: no match on `Expr`, no `List.at!`, no passing of `prog`.
`fac0` is an ordinary Unison combinator, so it gets hot and the JIT compiles it like any
hand-written definition. The recursion `Call 0 ...` closes into a loop because the second
time the specializer meets `eval prog <body of 0> <dynamic>`, it reuses the residual
function it is already building (see [Recursion](#recursion-and-termination)).

The classic smaller test: `specialize power 3` for
`power n x = if n == 0 then 1 else x * power (n - 1) x` gives `x -> x * (x * (x * 1))`.

## How it works

### Where the code comes from

Everything needed is already in the runtime:

- `intermed` in `CCache` ([`Machine/Types.hs`](Machine/Types.hs)) holds every loaded
  group's ANF (`SuperGroup`) before optimization, keyed by `Reference`. Local functions
  are already lambda-lifted into the group's `group` list, at index `k * 2^16` for the
  k-th.
- A closure is `GPAp cix comb args` ([`Stack.hs`](Stack.hs)). `cix` names the group and
  the combinator in it, and `comb` gives its arity.
- `cacheAdd0` loads new groups at runtime, numbers them, gives them native code cells and
  makes them visible to the JIT. `Code.cache_` already uses it.
- The interpreter itself, for anything computed from known values (see
  [What runs at specialization time](#what-runs-at-specialization-time)).

The builtin is a primop, `SPEC`, like `CACH` and `VALU`, because it needs the `CCache`.
`specialize f x` is a builtin whose body is `r = f x; SPEC r`
([`Builtin.hs`](Builtin.hs)).

### Abstract values

During specialization, each ANF variable is bound to one of:

| Form | Meaning |
| --- | --- |
| `Known v` | A fully known value: the runtime value itself (`Val`) |
| `Con ref tag [a]` | A known constructor whose fields are themselves abstract (partially known) |
| `Clo grp [a]` | A known combinator partially applied to abstract arguments |
| `Dyn v` | Unknown until run time: a variable of the residual code |

The two partial forms are what make interpreters specialize well. An environment that is a
list of `(name, value)` pairs with known names and unknown values is a `Con` spine with
`Dyn` leaves, so a lookup by a known name resolves at specialization time to the variable
that holds the value. A closure over a known function with unknown captures can still be
called directly and inlined. This is also what turns `List.map f` with a known `f` into a
loop with `f` inlined (the "specialization on function arguments" item in
[`JIT/optimization-ideas.md`](JIT/optimization-ideas.md), and `Floats.map` in numeric
code).

### The partial evaluator

It is an online partial evaluator over ANF. Online means it decides what to do from the
abstract values it actually has, with no separate binding-time analysis. ANF names every
intermediate result, so the evaluator walks a body once, producing residual ANF and
an abstract value for each binding. For each construct:

- **Literal, constructor application, closure creation (`AName`):** build the abstract
  value. Nothing is residualized unless it is needed later.
- **Match** on a `Known` or `Con` scrutinee: take the one branch and bind its fields. On a
  `Dyn` scrutinee: residualize the match and specialize each branch, with the scrutinee
  known to be that branch's constructor inside it. This is what makes
  `match env with Cons ...` after a dynamic test still useful.
- **Primop or foreign call:** if every argument is `Known`, run it on the interpreter
  (see the next sections). If that can't be done, or some argument isn't known,
  residualize it. The partial evaluator knows nothing about what any primop means.
- **Call of a known combinator**, or of a `Clo`: if every argument is `Known`, run it on
  the interpreter. Otherwise, or if running it fails, unfold it (specialize its body with
  these abstract arguments), or emit a call to a residual function specialized on the
  known parts of the arguments. The rule is in the next section.
- **Call of a `Dyn` function:** residualize the call. Arguments are *lifted*: a `Known`
  literal becomes a literal, a small `Known` or `Con` becomes constructor applications,
  a `Clo` becomes a closure of a (specialized) combinator, and anything else becomes a
  constant (see [Constants](#constants)).
- **Handlers and requests (`AHnd`, `AShift`, `FReq`):** residualized in the first version,
  with their bodies still specialized. See [Abilities](#abilities).
- **Let:** the binding is specialized, and its residual stays where it was, so effects keep
  their order and nothing unknown is computed twice. Only variables are substituted, never
  expressions.

### Recursion and termination

Unfolding every call fails on recursion that depends on dynamic data: `fac0` would unfold
forever. Each call to a known combinator gets a *key*: the combinator, plus the known parts
of its arguments with the unknown positions replaced by holes. The rules, after Similix (an
online partial evaluator for Scheme):

- **Unfold a call made with no dynamic match above it** in the residual function being
  built. That code runs once per call of the function anyway, so unfolding it can't loop
  unless the known computation itself loops, and fuel catches that.
- **Specialize a call made under a dynamic match.** It becomes a call to a residual
  function for its key, and the key is memoized. If the key has been seen before,
  including one still being built further up, the call reuses that function. That is
  how recursion in the interpreted program becomes recursion in the residual, and how
  `fac0` closes.
- **Clean up afterwards.** Specializing under every dynamic match makes many small
  functions: one for `Num 1`, one for `Mul Arg (Call ...)`, and so on. A pass after the
  specializer inlines every residual function that isn't recursive or is called once, and
  merges identical ones. That leaves `fac0` as shown above.

This terminates exactly when the set of keys is finite. Two kinds of known data appear in
keys:

- **Pieces of the input:** `x`, the captured values, and anything got from them by matching.
  These are finite in number, because they are subterms of values that exist. An
  interpreter's keys are of this kind (the program and its subterms), so an interpreter
  over a known program always terminates, with every key kept.
- **Computed values:** built during specialization by constructors and primops. A counter
  `go (n + 1) dyn` or an environment that grows with each `Let` is of this kind. Only these
  can grow without bound.

For computed parts, the safety net is a *whistle*. Along the chain of residual functions
being built for the same combinator, if a computed part keeps growing, it becomes unknown.
"Growing" is homeomorphic embedding. For literals it compares sizes: a `Nat`'s value, an
`Int`'s absolute value, a `Text`'s length. This is a well-quasi-order, which guarantees the
whistle eventually blows on any infinite chain. A known counter going down, as in `power 3`,
never blows it. It blows only after a part has grown several times in a row, a slack that is
adjustable, so that the environment of a few nested `Let`s keeps its known names.

This is the main risk in the design. A whistle that blows too early loses known data the
user expected to keep. One that blows too late makes too much code. The plan has a step
with tests aimed at both.

Budgets back all of this up: the total residual size, the number of residual functions,
and the fuel spent on known computation. A budget that runs out never makes `specialize`
fail. The calls still pending are residualized as calls to the original code, which is
always correct.

### What runs at specialization time

Known computation runs while specializing, possibly in places the real program would never
reach, such as a branch the dynamic data would not take. It runs on the interpreter, called
from Haskell on a fresh stack the way `preEvalTopLevelConstants` and the FFI callbacks call
Unison code. That reuses every primop, foreign function and replacement, at interpreter or
native speed, including a known call that handles its own abilities. Running it is safe
only if it has no effects, terminates and does not fail, which existing mechanisms check:

- **Effects and mutable state,** checked before running: the closure, everything it
  captured, and everything they reach must use no `Tracked` builtin (the IO-capable ones,
  which [`checkSandboxing`](Machine/Types.hs) already looks for, transitively, with the
  `sandbox` map) and no operation on mutable state (refs, mutable arrays, `TVar`, `MVar`,
  `Scope`), whose result could change between now and run time. `Ref.read` is untracked,
  so the second list is the specializer's own. Setting `sandboxed` on the cache would not
  do: a sandboxed cache replaces unsafe foreign calls when it loads code, not when they
  are called. The check is conservative, since it refuses code that could reach such an
  operation on a branch it never takes.
- **Abilities:** a request nothing in the call handles reaches the top of the nested
  evaluation and fails it.
- **Fuel:** the interpreter calls the ticker (`checkTicker`) on every call, so a profiler
  whose ticker counts down and aborts bounds interpreted work, with nothing added to the
  interpreter's loop. Native code never calls the ticker, so a time limit
  (`System.Timeout`, which interrupts native code at its preemption checks) is the
  backstop. A run cut off by the time limit makes the output depend on timing, never its
  meaning or its identity.
- **Errors:** a `bug`, a failed match or a division by zero is an exception, caught.

A call that fails any of these isn't an error. The partial evaluator works through it
symbolically instead, and whatever it can't compute is residualized, so an effect or an
error happens at run time, if the program gets there.

### Residual code

- The entry's reference is derived from a hash of `f` and `x` (see
  [What a specialized closure is](#what-a-specialized-closure-is)). The other residual
  groups get references from a hash of their contents (`ANF/Rehash.hs` already rehashes
  groups). Both use a namespace that can't collide with codebase hashes. A memo table from
  that hash to the result skips the specializer altogether on a repeat, and `cacheAdd`
  loads each group only once.
- They go through the normal pipeline: `optimize`, MCode emission, the explosion passes
  (known constructors and case-of-case clean up much of what partial evaluation leaves
  behind), and the JIT.
- Someone who called `specialize` expects the result to be hot. The new group is therefore
  queued for compilation at once, with no wait for the call threshold. This is
  configurable.
- A residual group can itself be specialized again, since its ANF is in `intermed` like any
  other group's.

### Constants

ANF has no node for an arbitrary value, only literals. A known value too big to rebuild
with constructors (a large list, an array, a closure over opaque state) has two possible
homes:

1. **As captured arguments** of the returned closure, threaded as extra parameters to the
   residual functions that use them. This needs no new IR. For interpreters it is rarely
   needed, because the program is consumed entirely.
2. **As a constant node** that refers to a per-process constant table, allowed only in
   generated code and never serialized (the JIT already has a constant pool). This is
   cheaper per call, but it is a new ANF form that every pass must handle.

(1) first, (2) if measurements ask for it.

## What makes an interpreter specialize well

Partial evaluation is only as good as the separation of known from unknown in the code
being specialized. The doc for `specialize` should say this plainly:

- Keep the program and the run-time data in separate arguments, or in partially known
  structures (an environment of known names and unknown values). Don't put them together
  in one opaque value.
- Keep the program in immutable data. Reading a `Ref` that holds the program gives an
  unknown.
- A dispatch on a value computed at run time but known to come from a small set (a variable
  index from a known program) specializes. A dispatch on genuinely dynamic data stays a
  match.

`runtime.ir` (below) shows what came out. A diagnostic, `UNISON_SPECIALIZE_DEBUG=1`, also
lists the places where a known value became unknown, and why: a budget, an embedding, or
an impure primop.

## Steering and inspecting

### `specialize.boundary`

```unison
specialize.boundary : (a ->{g} b) -> a ->{g} b
specialize.boundary f x = f x
```

It means `f x`. Under `specialize`, the call stays a call:

- The specializer does not inline `f`'s body. It doesn't generate a specialized copy of
  `f` either, and it doesn't run the call, even when `f` and `x` are both fully known.
- `f` and `x` are still specialized as values. A known `f` becomes a direct call to that
  function, which the JIT can make without going through a closure. `x` is lifted like any
  argument: a literal, constructors, or a constant.
- The result is unknown from then on.
- With more arguments, the call `specialize.boundary f a b` stays the single call `f a b`,
  not a call that returns a closure followed by an indirect call. If `f` is already
  unknown, the boundary changes nothing.

Uses:
- **Slow paths and big helpers** that aren't worth copying into every specialization, such
  as error reporting or a pretty-printer.
- **Code that should be compiled once and shared.**
- **Stopping precomputation:** `specialize.boundary (n -> List.range 0 n) 1000000` stays a
  call, instead of a million-element constant in the output.

Outside `specialize` it costs nothing. `intermed` holds each group's ANF before
optimization, which is what the specializer reads, so it sees the boundary. The normal
optimizer (`ANF.optimize`) inlines the boundary into a plain call before code is emitted.
The specializer's own output keeps the boundary calls, so specializing a specialized
function again still respects them.

### `runtime.ir`

```unison
runtime.ir : a -> Text
```

It shows what the runtime will run for a value:

- **A closure:** the values it has captured, then the optimized ANF of its group, the ANF
  that MCode is emitted from. For a
  specialized closure, the output also includes every generated group reachable from it.
  Codebase definitions it calls are shown by name, not expanded.
- **Any other value:** its reflected form (`ANF.Value`).

It is shown as `ir.optimized` in ucm shows code: the group as [`loadBatch`](Machine.hs)
optimizes it, after the explosion passes ([`rewrittenLambdas`](MCode.hs)), so workers and
join points appear too. The printer is a field of `CCache`, `irPrinter`: ucm installs the
printer of `ir.optimized` ([`PrettyANF.hs`](../../../ucm/Unison/Runtime/ANF/PrettyANF.hs),
with the codebase's names, `irPrinterFor` in
[`Interface.hs`](../../../ucm/Unison/Runtime/Interface.hs)), and a runtime without ucm uses
`plainIR`, which names references by hash. A value that can't be reflected (a mutable
reference) is named, not shown.

The text depends on the runtime version and the `UNISON_OPTIMIZATIONS` settings, so it is
for debugging and for golden transcripts, not for program logic. It is like `Debug.toText`,
and it is `Tracked`, as that one is.

## Abilities

Interpreters often use abilities themselves, for exceptions, state or the interpreted
language's I/O. In the first version, handlers and requests are residualized, so an
interpreter that installs a handler per evaluation step keeps that cost. The second step is
handler specialization. When the handler is known and the request is performed in its
scope with nothing unknown in between, the handler's case for that request is inlined at the
request. For affine handlers that is a call, and `ANF/Optimize.hs` already rewrites such
handlers. Non-affine handlers, which capture continuations, stay residual.

## Limits

- **Only the first projection.** The specializer is Haskell, so `specialize specialize eval`
  cannot produce a compiler (the second projection), and there is no compiler generator
  (the third). Self-application would need the specializer written in Unison, over a
  first-class view of ANF, and it would specialize far more slowly. The first projection
  already delivers compiled code. What the second would add is lower cost per program,
  and `specialize` cost should be small next to the JIT's compile time anyway.
- **Code is never unloaded.** A program that specializes on an unbounded stream of distinct
  values keeps growing its code cache, as `Code.cache_` does now. The memo table at least
  makes repeats free.
- **Mutable state stays unknown.** That includes `Ref`, mutable arrays and anything behind
  IO.

## Implementation notes

- **A builtin that returns a function from a primitive yields it as a variable.** A
  primitive in tail position yields its slots (`Yield (VArgV n)`), and the interpreter
  applies only a yielded variable (`VArg1`) to the pending arguments of an over-applied
  call. So `(specialize f x) y` lost `y` until the builtin bound `SPEC`'s result and
  yielded that.
- **A call inside a builtin needs an indirect let.** A direct let of a call is an
  internal error in the emitter. `specialize` numbers its let 1, the only one in its body,
  and the inliner never inlines a body with an indirect let, so the number can't collide
  with a caller's.
- **Loading from inside an instruction:** `specializeClosure` loads with `cacheAdd0` on
  the cache with its profiler replaced by `()`, since the instruction's code has no
  profiler constraint. A generated group is never a cached value, so loading it runs
  nothing.

## Plan

1. **The builtin, as identity.** Done. `specialize`, `specialize.boundary` and
   `runtime.ir`. The generated group is the original lambda, unchanged. Covered by
   [`specialize.md`](../../../../unison-src/transcripts/idempotent/specialize.md): the
   arity cases, effects in `f x`, equality, the version 5 round trip, a mutable value as
   `x`, and `runtime.ir`.
2. **Running known code.** Call a known closure on known arguments on the interpreter,
   after the check for effects and mutable state, under fuel and a time limit, returning
   the result or "can't". Tested on its own: a pure call, an effect, a mutable read, a loop that runs
   out of fuel, a `bug`.
3. **First-order partial evaluator.** The walker over ANF with `Known` and `Con`: matches
   on known values, calls run with step 2 when everything is known, unfolding, residual
   functions keyed and memoized, budgets, and loading through `cacheAdd0`.
   `specialize.boundary` is respected here. The tests are `power`, a closed expression
   evaluator, and the `eval` example above. Each checks both the result and the shape of
   the residual (with `runtime.ir`).
4. **Higher order.** `Clo`, calls of known closures, and lifting of closures. The tests
   are `List.map` and `foldLeft` with known functions, and an interpreter written in
   continuation-passing style.
5. **Benchmarks.** An interpreter for a small language running a few programs, measured
   under the JIT in three forms: interpreted, specialized, and the same program written by
   hand in Unison. Success is "specialized" close to "by hand".
6. **Generalization and diagnostics.** Homeomorphic embedding, the list of known values
   lost (`UNISON_SPECIALIZE_DEBUG`), and tests that are meant to hit each budget.
7. **Abilities.** Inline known affine handlers.

## Open questions

- **Granularity of the memo key:** the hash of the whole known argument, or something
  cheaper. Hashing a large program on every `specialize` call is linear, which is fine
  once, but not if `specialize` sits inside a loop.
- **Constant node or captured constants** (see [Constants](#constants)).
- **Fuel and budget defaults,** and whether they should be adjustable per call (a variant
  of the builtin taking options) or only per process.
- **Whether `specialize` should block** until the native code exists, for benchmarks and
  predictable latency, or return at once and let the JIT catch up.
- **Eager or lazy specialization when a `Specialized` value is read.** Eager now.
- **Name:** `specialize` or something under `Function.`.
