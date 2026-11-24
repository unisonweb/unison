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
- **Identity and the memo table:** the reference of a specialization is a hash of the
  closure with each value that can't be serialized (a mutable reference, a handle) written
  as one placeholder ([`specializedRef`](Machine.hs)). The code doesn't depend on which
  such value it is, because they never enter known computation (`runKnown` refuses values
  holding one), so closures that differ only in their mutable values share the generated
  code, while each holds its own values, which keeps their identities apart: equal for the
  same reference, not for different ones. Specializing in a loop over a closure holding a
  ref generates code once. A constant the walk took out of a mutable value (a ref in a
  record the closure holds) is the closure's own; such a specialization is marked in the
  table, and a repeat walks the code again for its constants, loading nothing.
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
- **Under a dynamic match, unfold a call too, unless it could go on forever**
  ([`call`](ANF/Specialize.hs)). Unfolding keeps what the call gives known to the code
  after it: a closure it builds, the constructor of its result. A call of a residual
  function gives an unknown value, and an interpreter that gets one where it had a known
  function has to interpret that function's body.
- **It could go on forever when the same combinator is being unfolded or built around
  the call with a key no bigger** (the whistle, below), or already has a residual
  function for this key. Then the call becomes a call to a residual function for its key,
  and the key is memoized. That is how recursion in the interpreted program becomes
  recursion in the residual, and how `fac0` closes: the interpreter going into a part of
  its program unfolds, and coming back round to the same code it makes the function that
  is the loop.
- **A call that knows less than one around it counts as no bigger.** `go [] xs` calling
  `go (acc :+ f x) rest` has an unknown accumulator where it had a known one: that call
  is the general one, the loop, and unfolding it would only peel off a round.
- **Clean up afterwards.** A pass after the specializer inlines every residual function
  that isn't recursive or is called once. That leaves `fac0` as shown above.

This terminates exactly when the set of keys is finite. Two kinds of known data appear in
keys:

- **Pieces of the input:** `x`, the captured values, and anything got from them by matching.
  These are finite in number, because they are subterms of values that exist. An
  interpreter's keys are of this kind (the program and its subterms), so an interpreter
  over a known program always terminates, with every key kept.
- **Computed values:** built during specialization by constructors and primops. A counter
  `go (n + 1) dyn` or an environment that grows with each `Let` is of this kind. Only these
  can grow without bound.

For computed parts, the safety net is a *whistle*. Along the chain of calls being unfolded
and residual functions being built for the same combinator, if a computed part keeps
growing, the call stops being unfolded, and in a residual function the part becomes
unknown.
"Growing" is homeomorphic embedding. For literals it compares sizes: a `Nat`'s value, an
`Int`'s absolute value, a `Text`'s length. This is a well-quasi-order, which guarantees the
whistle eventually blows on any infinite chain. A known counter going down, as in `power 3`,
never blows it. It blows only after a part has grown several times in a row, a slack that is
adjustable, so that the environment of a few nested `Let`s keeps its known names.

This is the main risk in the design. A whistle that blows too early loses known data the
user expected to keep. One that blows too late makes too much code. The plan has a step
with tests aimed at both.

Budgets back all of this up: the size of the generated code, the number of residual
functions, and the fuel spent on known computation. A budget that runs out never makes
`specialize` fail: the result is the original code, which is always correct.

The size budget is a multiple of the size of the input
([`specLimits`](Machine.hs): 20 times the nodes of the function's code plus the values the
known argument is made of), since code specialized on a value grows with it, as an
interpreter's does with its program. The specializer counts the lets and matches it
generates ([`emitted`](ANF/Specialize.hs)). Past the budget:
- **A fused handler in force is given up on.** Fusing can copy the rest of the walk into
  the arms of an unknown match (where a join point can't be shared, see
  [Abilities](#abilities)), which is what grows the code. The walk rolls back to its
  `handle` and keeps the handler.
- **With none in force, the specialization starts again with no handler fused**
  ([`specializeLambda`](ANF/Specialize.hs)): the handlers that grew the code may be
  finished by the time the budget runs out.
- **If that runs out too, the result is the original code.**

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

ANF has no node for an arbitrary value, only literals. A known value that isn't a literal
or text is a *constant*: a parameter of the entry and of every residual function, which
calls pass along ([`threadConsts`](ANF/Specialize.hs)). The specialized closure holds the
values: first the arguments of the closure it was made from, which are constants already,
then any others the walk needed. CCache's `specializations` records both counts, so that
reflection and the decompiler can drop the extra ones.

A constant node referring to a per-process table, allowed only in generated code, would
save passing constants around, at the cost of a new ANF form every pass must handle. Not
needed so far.

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

Generated code keeps the program's variable names. Every variable the specializer makes
comes from `freshNamed` ([`Specialize.hs`](ANF/Specialize.hs)), and the caller names it:
- **The program's name**, when the variable stands for one of the program's: a parameter,
  a let's binder, a field, a handler's binder. A residual function's parameters take the
  original function's parameter names.
- **The function's name with a prime**, for a residual function: `foldLeft'` for one made
  from `foldLeft`, so that it never reads as the function it came from, which the code may
  also call. The name is a local function's binder (a user's name only), or else the
  group's name in the codebase. The cache keeps those names (`combNames` in `CCache`):
  ucm records them as it loads code (`recordNames` in
  [`Interface.hs`](../../../ucm/Unison/Runtime/Interface.hs)), and code loaded at run time
  has none.
- **A synthetic name** otherwise: `fn` for a function with no name, `lit`, `con`, `clo`, a
  call's result `r`, a constant `c`, and in state passing `p`, `s`, `rest`, `done` and
  `out`.

The printer makes the names unique by numbering them: two residual functions made from
`evalA` are `evalA'` and `evalA'1`.

The text depends on the runtime version and the `UNISON_OPTIMIZATIONS` settings, so it is
for debugging and for golden transcripts, not for program logic. It is like `Debug.toText`,
and it is `Tracked`, as that one is.

## Abilities

Interpreters often use abilities themselves, for exceptions, state or the interpreted
language's I/O. What matters for specializing is how much is known, and how a handler uses
its continuation, not whether the handler is affine:

- **Everything known: it runs.** A subterm whose free variables are all known, a `handle`
  of any kind included, runs on the interpreter ([`runIfKnown`](ANF/Specialize.hs),
  [`runTerm`](Machine.hs)), which implements every handler, multi-shot ones too. This
  applies to any code the walker doesn't model.
- **The handler stays, the body is specialized.** Otherwise a `handle` is kept, with the
  code under it specialized and its requests kept as requests; the handler's own code (its
  match on requests, its `shift`) is specialized like any other. An interpreter that uses
  an ability for errors loses all of its interpretive overhead this way, with its errors
  still real requests.

  The body under a kept `handle` is a call, as the compiler makes it
  ([`handledBody`](ANF/Specialize.hs)): a residual function of the variables the body uses
  from outside. A continuation captured under the handler holds the stack down to the
  `handle` and no further, and a variable is found by its distance down the stack, so code
  that runs after a request must not use a variable of the frame the `handle` is in: the
  continuation may be resumed with more or less stack below it. A body that only builds
  values and then makes one call stays inline.
- **Fusing the handler away.** A `handle` whose handler is a known function, over
  abilities no builtin requests (not `IO` or `Exception`), is fused: no `handle` is made,
  and a request the walk reaches is handled as it goes ([`request`](ANF/Specialize.hs)).
  The handler is unfolded on a static request (`SReq`), whose match picks the case and
  whose `shift` binds a static continuation (`SKont`): the rest of the walk from the
  request. Resuming it (a call of `jumpCont` on it) carries on the walk, with the frames
  between the request and its handler put back, and the resumption site as where the body
  returns: the walker keeps the handlers in force (`sFrames`) and where each body returns
  (`sTargets`, a metacontinuation) in its state. So a case that doesn't resume drops the
  rest of the body, `handle k v with h'` carries on under `h'`, and a plain `k v` returns
  to the case. Under a fused handler, calls are unfolded rather than made residual
  functions, so that every request sees all of its continuation.

  For the same reason the code after an unknown match can't be a let's body there: a
  request in an arm must see it. It becomes a join point (below).

  It is all or nothing for a `handle`: anything in its scope that could perform its requests
  unseen escapes (`Escape`) back to the `handle`, which rolls the walk's state back and keeps
  the handler. That is a call of an unknown function, code kept as it is, a builtin that
  calls back into Unison code (`jumpCont`, a fork, a transaction: read off the builtins'
  own code), a request behind a kept handler, a second resumption of the same continuation,
  and a residual function, while a handler is fused or a captured continuation waits
  (its code would land in another lambda, out of the request's scope). `UNISON_SPECIALIZE_DEBUG=1`
  says which it was for each handler kept. So straight-line
  code fuses, and so does an interpreter over a program with no loops: the transcript has
  a state handler becoming arithmetic and an abort becoming a branch.

  A loop in a fused handler's scope becomes a residual function that passes the handler's
  state ([`residualCall`](ANF/Specialize.hs)): the handler is one more argument, the body is
  walked under a frame of its own that owns no target (a segment: the body returns to its
  caller, not to the handler), and the function returns `Left v` from a case that didn't
  resume, `v` being the `handle`'s value, or `Right (result, state)` at its end, where the
  handler in force must have the shape it was passed in and every resumption must have
  re-installed a handler as its case's last act (`returnRight`); the caller matches, and
  goes on after the `handle`, or with the handler rebuilt from the state (`resultMatch`).
  A state handler around a loop becomes a loop taking and returning the state, and an abort
  around a loop becomes a loop that returns the abort as a value.

  Still to do: cases that resume more than once, which need the rest of the body as a
  residual function up to the `handle`; more than one fused handler around a loop; and a
  resumption whose result the case keeps working with, inside a loop.

## Join points

An unknown match has code after it: the rest of the let it is bound in, and of whatever
that is in. The specializer has that code as a continuation, and what it does with it
decides how much is known after the match ([`joined`](ANF/Specialize.hs)).

- **The arms are walked first.** Where an arm reaches the code after the match, it leaves
  a placeholder and records the value it gives.
- **If no arm's value has anything known in it,** the match is bound to a variable and
  the code after it follows, once, as a let's body. This is the common case, and the
  output is what it would be without join points.
- **Otherwise the code after the match becomes a join point,** a residual function that
  each arm calls at its end. The arms' values are generalized together
  ([`lubAbs`](ANF/Specialize.hs)): what they agree on stays known, and each part where
  they differ becomes a parameter. A match whose arms all give `N` of some number goes on
  knowing the `N`, where a variable would have lost it.
- **Only what costs little is generalized:** a part that is unknown in some arm already,
  numbers that differ, constructors with no fields (`true` and `false`). Arms that differ
  in anything else known, such as which piece of an interpreted program they give, go in
  separate groups, each with a join point of its own that knows which it has. Code that
  went on without knowing would have to interpret it.

Under a fused handler ([Abilities](#abilities)) there is always a join point, since the
code after the match can't be a let's body, and it depends on the handlers in force:

- **An arm can share a join point only if its handlers at its end stand for the ones at
  the match** (`standFor`): the same frames, or ones that cases installed as their last
  act to go on under, which differ in their state. To tell, what follows each fused
  `handle` has a name (`KOuter`), which a handler re-installed in a case's last position
  inherits.
- **The handlers' states are generalized with the values** and passed to the join point,
  whose body is walked under the handlers at the match with those states.
- **Any other arm gets its own copy** of the code after the match.
- **A continuation resumed in an arm and again after the match** is resumed twice, which
  escapes as it did when the arm held a copy.

A join point uses variables of the function it is in. When the walk is over each gets
them as parameters ([`freeNeeds`](ANF/Specialize.hs)), before inlining, which renames the
variables of the copies it makes.

A join point is a function, so a call of it costs what a copy of the code doesn't. The
clean-up pass ([`inlineFns`](ANF/Specialize.hs)) removes the ones that don't pay:
- **One with a single call goes into it.** Most are like this: the other arms of the match
  end without reaching the code after it (an abort, a failure).
- **One with several calls is copied into them if the copies add at most 100 nodes,** the
  innermost first, so that each is judged with the join points it calls already in it.
  Copying an outer one first would double the calls of the next, and so on down, which is
  the growth join points are there to stop.
- **Join points go before any other function.** A loop's function copied into the join
  points it calls would give each of them more calls, and leave a call per join point in
  every round.

`UNISON_SPECIALIZE_JOIN=off` turns join points off, to compare: the code after a match
under a fused handler is then copied into every arm. Over 100 programs of the
[differential tester](#testing) with its first interpreter, that was 222,000 lines of
generated code against 85,000.

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

## Testing

[`specialize.md`](../../../../unison-src/transcripts/idempotent/specialize.md) has the
cases written by hand, with the code each gives.

[`specialize-differential.md`](../../../../unison-src/transcripts/idempotent/specialize-differential.md)
tests the rule itself, that `specialize f x` behaves as `f x`, on generated programs. It
has an interpreter for a small language, and specializes it on random programs: a known
program gives residual code in that program's shape, so random programs give varied code.

- **The language** is a dynamically typed lambda calculus over `Nat`: functions, two
  bounded loops, and effects (`Perform`, `Handle`, and `Resume` in a handler's clause). A
  handler's clauses are data, so the interpreter's handler, specialized, becomes that
  handler.
- **The interpreter's environment is a data type of its own,** not a `List`. The
  specializer sees its shape, so a variable is found while specializing, and what it
  finds can be a known function or continuation. With a `List`, a clause's continuation
  had to go through a builtin as a value, which it can't, and every handler whose clause
  ran was kept.
- **The generator** is typed, so a program never fails with a type error, and every
  program ends. A clause uses its continuation in each of the ways one can: not at all, as
  its last act, with more work after, twice, and inside a function.
- **The checks,** each against `f x`: `specialize f x`; specializing the first input too;
  the specialized closure after a round trip through serialization; and specializing twice
  giving equal closures. A specialized run that fails counts as a disagreement.
- **A failure is shrunk** to a smaller program of the same type that still fails, trying
  the biggest cuts first, and reported with its seed.
- **The report counts** how many cases the specializer gave up on, kept a handler in, and
  fused every handler in, read off `runtime.ir`. A run where most cases gave up tests
  little.

The transcript checks seeds 0 to 49 and the seeds that found bugs. To try others, change
the arguments of `check`: 300 seeds take about 100 seconds.

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
2. **Running known code.** Done: [`runKnown`](Machine.hs) calls a known closure on known
   arguments on the interpreter, after the check for effects and mutable state
   (`knownRunnable`), under fuel and a time limit, returning the result or "can't".
   Tested in [`Specialize.hs`](../../../tests/Unison/Test/Runtime/Specialize.hs): a pure
   loop, a tracked builtin, a mutable read on a branch never taken, a `bug`, a loop that
   runs out of fuel, and fewer or all arguments.
3. **First-order partial evaluator.** First version done:
   [`ANF/Specialize.hs`](ANF/Specialize.hs), with `Known`, `Con`, `Clo` and `Dyn`; matches
   on known values, and on unknown ones with the constructor known in each arm; calls
   run with step 2 when everything is known, unfolded with no unknown match above them,
   and residual functions keyed and memoized under one; primitives run through
   [`runPrim`](Machine.hs); `specialize.boundary`; budgets, whose exhaustion falls back to
   the original code. `power`, a closed expression and the factorial interpreter are in
   the transcript, which checks the residual with `runtime.ir`. A clean-up pass after the
   walk ([`inlineFns`](ANF/Specialize.hs)) inlines residual functions that aren't
   recursive and are called once or small, and passes each residual function only the
   constants it uses (`constNeeds`). Still to do here: handlers and requests, and lets of
   several values, which are kept as they are; and the first round of every loop is
   peeled (the walk unfolds a recursive call until the first unknown match), which
   doubles a loop's code.
4. **Higher order.** Done for the common cases: a known closure called with unknown
   arguments is unfolded like any known function, a known builtin is called directly, and
   a closure of a local function over partially known values becomes a closure of a
   residual function specialized on the known part. `map` and `foldLeft` with known
   functions are in the transcript, and so is an interpreter in continuation-passing
   style, which needs the whistle of step 6.
5. **Benchmarks.** Done: [`specialize-benchmarks.md`](../../../../unison-src/transcripts-manual/specialize-benchmarks.md),
   results in [JIT/benchmarks/2026-11-22-specialize.md](JIT/benchmarks/2026-11-22-specialize.md).
   Specialized interpreters land within 15% of the programs written by hand, and known
   function arguments inlined into loops match hand-written loops under the JIT.
6. **Generalization and diagnostics.** The whistle is in: when a residual function's key
   embeds the key of one being built further up for the same function
   ([`embeds`](ANF/Specialize.hs), with sizes for literals), the parts where they differ
   are generalized to unknown and passed from the call as values (`generalize`). Unlike
   the scheme in [Recursion and termination](#recursion-and-termination), it is checked
   only between residual functions being built, and it doesn't yet distinguish pieces of
   the input from computed values. `UNISON_SPECIALIZE_DEBUG=1` says on stderr what each
   specialization made, its constants, or why it gave up, and how much known code has run:
   the runs, the ones cut off by the time limit, the ones that failed, and the groups in
   the code cache. Still to do: tests that hit each budget.
7. **Abilities.** Running known code with handlers, and keeping handlers with their
   bodies specialized, are done: the transcript has a state handler over known code
   becoming a constant, and an interpreter with an abort ability specialized inside its
   handler. Fusing handlers away is done for cases that don't resume or resume once as
   their last act, around straight-line code and loops (see [Abilities](#abilities)).

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
