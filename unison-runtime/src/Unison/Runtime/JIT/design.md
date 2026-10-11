# Unison JIT Design

Sep 26, 2026 · Paul Chiusano

This document is the model: what the JIT is, the invariants it keeps, and why. How each part is built today is in [internals.md](internals.md), organized by mechanism; how to build, run and test it is in [development.md](development.md); the measurements that shaped it are in [benchmarks/](benchmarks/) and the history in the branch's commits.

## Summary

This document describes the design of Unison's new LLVM-based JIT compiler, which can execute Unison code at native speeds.

A bit of history to motivate this design: we previously attempted a JIT that aimed to compile the entirety of the language to Racket Scheme (itself compiled at runtime to native code). This effort was ultimately abandoned for a few reasons: it was quite complicated to try to cover literally everything in the Unison language, especially the dynamic code-loading and serialization support used by Unison Cloud, and other than simple arithmetic loops, we never saw major performance wins over the interpreter. Certain functional code was actually *faster* with our tuned interpreter. It was also very much an "all or nothing" effort that couldn't be used until every Unison builtin had a Scheme implementation.

This new JIT uses a different approach that allows it to be useful immediately. The idea is to compile functions to native code with [LLVM ORC](https://llvm.org/docs/ORCv2.html). But rather than needing to cover the entirety of the Unison language and its builtins, we allow native-compiled functions to "exit" back to the interpreter. Thus, delimited continuations, abilities, and any other subset of the language which is hard to compile to LLVM will continue to work but will defer to the interpreter at runtime. The idea is similar to what's done in a tracing JIT, where branches "off trace" revert to the interpreter, but here we're applying the idea to a function-at-a time JIT.

A few tricky concerns to be aware of for the design:

* Haskell has a moving garbage collector, so native code should never retain heap pointers after returning control back to the interpreter. Otherwise those pointers could get relocated and the native code would be left with a stale pointer.
* Native code must preemptible in the same way as other Haskell functions.
* Native code should keep the same conceptual model of the stack being growable. We do not want perfectly cromulent functional programs to crash because of a randomly low / fixed C stack limit. 

**Goals**

- No change to language semantics, the continuation representation, or serialized code.
- Large speedups on most Unison code, especially straight line code that doesn't use fancy features like abilities or delimited continuations.
- Support only a subset of `MCode`, and grow it over time. Anything unsupported runs in the interpreter, with identical behaviour.

Note that code for a definition never changes, so compiled code never needs invalidating!

**Non-goals, for now**

- Compiling `Capture`, `Reset`, `Jump` or `Discard` natively. (Requests of affine handlers, and the handlers' cases, are native: [Ability requests](internals.md#ability-requests).)
- Making compiled code callable as an ordinary Haskell function, or making it follow GHC's calling conventions.
- Compatibility with the Racket / `run.native` backend. This backend is defunct.
- Caching compiled code on disk across runs. If compilation is fast enough, this may never be worth it.

## Background: the runtime the JIT builds on

Our existing machinery has already, before native compilation, converted the code to a set of  _supercombinators_ (or just "functions" in this document). A supercombinator is a function definition in A-normal form that contains no inner functions or lambdas and which can only call other supercombinators. The body of a supercombinator is a value of type `MCode` (for "machine code"). 

Here are the relevant types:

- **MCode** (`Unison.Runtime.MCode`). Each supercombinator is a `GSection` tree of instructions. Instructions include things like `DMatch` (pattern matching on a value), `App` (call an unknown function), `Call` (fully saturated call to known function), `ForeignCall` (call a foreign function), `Let` (evaluate and push result to stack), and so on.
- **Stack** (`Unison.Runtime.Stack`). It has three pointers, `ap` (arg pointer), `fp` (frame pointer) and `sp` (stack pointer), an unboxed `MutableByteArray` (`ustk`) and a boxed `MutableArray Closure` (`bstk`). Both arrays are unpinned and are reallocated when the stack grows. Instructions address slots relative to `sp`, so the layout at each program point is fixed statically. `ap` matters for frames: `saveFrame` records a frame's size as `sp - fp` and its pending-argument count as `fp - ap`, and `restoreFrame` rebuilds `ap` from that count.
- **Builtins.** Some builtin functions like arithmetic have direct instructions (via `Prim1` and `Prim2`) but most others are `ForeignCall` instructions that run a Haskell function and push results to the stack.
- **Values.** `Closure` wraps `GClosure`: `GPAp`, `GEnum`, `GData1`, `GData2`, `GDataG`, `GCaptured`, `GForeign`, `GUnboxedTypeTag`, `GAffine`, `GBlackHole`. Unboxed values are a `ustk` word plus a type-tag closure in the matching `bstk` slot.
- **Continuation** `K`. A chain of frames: `Push` (frame size, pending args, resumption `CombIx` + `RSection`, stack guard), `Mark`/`AMark` (prompts) and `Local`/`Keep`/`CB`. `Capture` copies stack segments plus `K` frames up to a prompt.

## Exits: how native code covers a subset of the language

Native code, once compiled, is entered through a foreign call, using the C calling convention. It allocates Haskell heap objects just like any other compiled Haskell code. For instructions that can't be run in native code, the function returns in such a way that the interpreter knows to run that instruction and then re-enter native code right after. This is called a "call-out"; see [re-entry points](#re-entry-points).

Every native-compiled function has a wrapper with the same signature. This uniform signature is what the interpreter calls:

```c
typedef STATUS (*UnisonNativeFn)(Ctx *ctx, int64_t ap, int64_t fp, int64_t sp);
```

(Each supercombinator also gets compiled as a separate *worker* that takes arguments as ordinary parameters and returns results via unboxed LLVM tuples. The wrapper calls the worker, and within a batch, workers can call each other and themselves directly without having to stash values on the Unison stack. See [Calls](#calls).)

`Ctx` is a per-thread value holding what native code needs from the runtime (the addresses of the Unison stacks and of the constant pool, the limits it checks against) and where it writes what the runtime needs back (the stack pointers on exit, since a C function can only return the one `STATUS`). The `ap`, `fp` and `sp` *arguments* are the values on entry; the `Ctx` fields are the values on exit.

`STATUS` is an `int64_t`: `OK = 0` means the function returned normally; `EXIT_ERROR = -1` means it returned with an error, details stashed in `Ctx` (other negative values are reserved); positive values are an exit, a 1-based index into the global `Exit` table.

Before it returns an exit, native code puts the frame back on the Unison stack, since the interpreter continues from there: the slots the resumed code will read (a liveness walk decides which), and the stack pointers into `Ctx`. The site does this with one call to a C routine in the runtime, passing the live values as arguments and a constant descriptor saying where they go; generating the stores themselves made the exit paths most of a function's code, and nearly none of them ever run. See "Write-back" in [internals](internals.md#the-generator).

The exits table tells the interpreter what to do when native code returns an exit. It is an ordinary Haskell structure, since an `Exit` holds Haskell heap objects (`RSection`, `GInstr`). Native code never reads it: it only returns an index into it, as a constant baked into the generated code. (Each supercombinator has a fixed number of possible exits, known at compilation time, so it's no problem to give each supercombinator a range in the global exits table.)

```haskell
data Exit
  = Resume (RSection Val) -- interpret from this section
  | CallOut (GInstr Val) (RSection Val) Int (Ptr NativeCell)
    -- run one instruction in Haskell (it pushes this many values), then
    -- call the function in this cell, which continues with the section
    -- after the instruction. The section is interpreted instead when the
    -- cell is still empty (its code is generated on demand, or not yet
    -- installed) or when the instruction pushed a different number of
    -- values; if it raised an exception, the section is the continuation
    -- after the handler.
  | GrowStack Int (Ptr NativeCell)
    -- grow the Unison stack to fit a frame of this size, then call
    -- the function in this cell again
  | Reenter (Ptr NativeCell)
    -- a suspension point: native code has handed control back so the
    -- Haskell runtime can switch threads or collect garbage; when this
    -- thread next runs, call the function in this cell again
```

(The real type, in `Exits.hs`, also records the combinator each exit belongs to, for statistics. The exits name [native code cells](#native-code-cells) rather than code pointers because code is installed while the program runs: an exit can be taken before its continuation's cell has been written, and a call-out's re-entry function may not have been generated at all yet.)

`Resume` is straightforward, it's just interpreted code to run on resume. `CallOut` is needed so that a single exit in the middle of a function doesn't cause the whole rest of the function to run interpreted: the interpreter runs the one instruction and then resumes using the native re-entry function (if its cell is non-null) or the given `RSection` (if not).

| Situation | Exit kind |
| --- | --- |
| Any instruction without a native version | call-out, re-enter after the instruction |
| Unsupported control *section* (`Jump`, `Die`, …), or `Capture` / `Discard` / `Reset` | resume at that section |
| An ability request whose number the environment doesn't map to an affine handler ([Ability requests](internals.md#ability-requests)) | resume at the section |
| An affine handler's `InLocal` when the helper refuses the closure or as many environment switches are in force as an exit can hand back | call-out, re-enter after the instruction (the `Local` frame is then the interpreter's) |
| An exit of either kind inside a handler case whose environment switch is native | the exiting function also writes a `Local` frame record carrying the environment to switch back to, which the interpreter rebuilds as a `Local` frame |
| A call native code can't make, in a `Let` binding: the callee isn't compiled, or it's an `App` of a function value in one of the [cases left to the interpreter](#calling-function-values) | record the `Push` frame for the `Let` body, resume at the binding. Native code is re-entered at the `Let` body when the callee returns. |
| The same, in tail position | resume at the `App` / `Call` section |
| Unison stack needs to grow | `GrowStack`: the function exits at its entry check, before doing anything else. The interpreter grows the stack and calls the same function again. See [Stack growth and preemption](#stack-growth-and-preemption). |
| Preemption poll, or allocation budget used up | `Reenter`: the function exits at its entry poll. Once the Haskell runtime has done what it needed to, the interpreter calls the same function again. |
| Combinator returns | not an exit: status `OK`, which goes to `yield` |
| `Die`, or an error from a foreign call | not an exit: status `EXIT_ERROR`, with the message in `ctx` |

## The core rule

Whenever native code hands control back to Haskell, the Unison stack (`ustk`, `bstk`, `ap`, `fp`, `sp`) will be exactly as the interpreter would have left it at that `MCode` point. Native code may do other stuff internally, keeping values in registers, using a different representation of the call stack, etc, but must produce the correct Unison stack before returning.

This has some consequences:

- Native-compiled supercombinators that have to exit back to the interpreter will need to write to the stack and construct `K` frames on demand. At the time of compilation, we have enough information to be able to construct code that will do this for each possible exit point of the function. (Each continuation point is just an `MCode` subtree that's easily available because of the A-normal form nature of the code.)
- For native-to-native calls, the caller will check whether the callee is returning normally (because it completed) or if it is exiting back to the interpreter partway through the function. If exiting back to the interpreter, the caller will record what's needed to build its own `K` frame (see [Frames](#frames-native-code-records-haskell-builds)) and then itself exit.
- `Capture` (for capturing a delimited continuation) won't ever be run in native code. Instead, at every point where we would capture, the native code will be exiting back to the interpreter, and producing ordinary stack segments and `K` frames as it unwinds. Thus, we don't need LLVM code to have a concept of continuations at all, it's just another unsupported language feature that exits to the interpreter whenever it's hit.

A corollary, since native code never calls into Haskell: Haskell exceptions never have to unwind through native frames. Errors come back as `EXIT_ERROR` with a payload that the interpreter rethrows.

## Re-entry points

A compiled function can exit to the interpreter for a few reasons:

* To grow the stack
* To allow for GC or thread preemption
* A call-out to have the interpreter run `MCode` with no native equivalent

In each of these cases, we may have to resume the interrupted function midway through its execution.  For example, consider:

```
f n =
  x = g n      -- non-tail call
  x + 10       -- the Let body
```

If `g` returns normally, native `f` just carries on into `x + 10`, and no re-entry point is involved. If `g` exits to the interpreter, we need a re-entry function for the continuation from that point in `f`.

We generate re-entry functions eagerly for resumptions after a call-out (since these locations are statically known), but the others are generated lazily, if and when they are needed.

Properties of re-entry points:

- **Each one is a `UnisonNativeFn`.** It starts by reading its locals from the Unison stack slots, which is always possible because of [the core rule](#the-core-rule), and begins with the same stack check as any native function.
- **No code is duplicated.** The code after a call-out lives only in the re-entry function.
- **They're optional.** A `Let` with no re-entry point is resumed in the interpreter, which is always correct. So they can be added gradually.
- **Native code never looks one up;** only the interpreter does. A call-out's re-entry point is a cell held by its `Exit`. A `Let`'s re-entry point is the native code of the `Let` body's own combinator: the MCode emitter already makes every `Let` body a combinator whose arguments are the whole frame, so calling it is resuming at the body. Its [native code cell](#native-code-cells) is carried by the `Let` node and by the `Push` frame.
- **A re-entry point inside an inline binding sees the interpreter's frame.** When native code exits from inside a binding it compiled inline, the frame records describe a frame for the binding, so the code that re-enters there is generated as if entered with the interpreter's frame pointer at the binding's base; slot offsets stay those of the enclosing function, so the body finds its values where it expects them. (The mechanism, the *frame base*, is in [internals](internals.md#frame-records-and-the-frame-base).)

## Frames: native code records, Haskell builds

**What a `K` frame is.** `K` (in `Stack.hs`) is the interpreter's continuation: a linked list, allocated on the Haskell heap, that says what to do after the current code returns. Each constructor in the list is one frame:

- `Push`: "when the current call returns, continue at this section." It records the caller's frame size, pending argument count, a stack guard, and the `CombIx` + `RSection` to resume at. The interpreter pushes one at every non-tail call (`Let`).
- `Mark` / `AMark`: prompts installed by ability handlers (`Reset`). `Capture` copies the list up to the matching prompt.
- `Local`, `Keep`, `CB`, `KE`: saved handler state, values kept alive, callbacks, and the empty continuation.

**The problem.** When the interpreter makes a non-tail call, it allocates a `Push` frame first, so that `K` always says where to continue afterwards. Native code doesn't do this. When native `f` calls native `g`, the fact that "`f` continues after `g` returns" exists only as a return address on the C stack. If `g` returns normally, that's all that's needed, and no `Push` frame is ever allocated. But if `g` has to exit to the interpreter, the interpreter needs a `K` that includes a frame for `f`, or it won't know to continue with the rest of `f` when `g`'s work is done.

**The approach.** Native code can't construct a `Push` frame itself. A `Push` is a Haskell heap object that points to a `CombIx`, an `RSection` and the rest of `K`, and we'd rather native code know nothing about how `K` is represented. So the work is split:

- Native code writes down a *frame record*, three integers, for each frame that needs to exist.
- After native code returns, the interpreter turns the frame records into real `Push` frames.

A frame record contains:

| Field | Where it comes from |
| --- | --- |
| `Let` body id | a constant in the generated code, identifying which `Let` body to continue with |
| frame size | `sp - fp` at the call site, a runtime value |
| pending-argument count | `fp - ap` at the call site, a runtime value |

These are the same two sizes the interpreter's `saveFrame` computes. The remaining fields of a `Push` (the `CombIx`, the stack guard and the `RSection`) are fixed for a given `Let`, so they're kept in a Haskell-side *frame table* indexed by `Let` body id, filled in when the function is compiled. This is the same arrangement as the exits table: native code only ever holds an index.

**Example.** The interpreter, whose continuation is `k0`, calls native `a`. Then `a` calls `b`, `b` calls `c`, and `c` reaches a `Capture`, which it can't run.

| Step | What happens | Frame records in `Ctx` |
| --- | --- | --- |
| 1 | `c` writes its live values to the stack and returns exit index 17 | none |
| 2 | `b` sees a non-`OK` status, records its frame, returns 17 | `b` |
| 3 | `a` sees a non-`OK` status, records its frame, returns 17 | `b`, `a` |
| 4 | The interpreter builds `Push b (Push a k0)` | |
| 5 | The interpreter looks up exit 17 and resumes at the `Capture` with that `K` | |

At step 5 the stack and `K` are exactly what they'd be if the interpreter had run `a`, `b` and `c` itself, and the C stack holds nothing. `Capture` works unchanged. Later, when `yield` pops the frame for `b`, it finds the [re-entry point](#re-entry-points) for that `Let` in `b` and goes back into native code. Nothing is paid on the normal path beyond the status check after each call; the record order, inlined `Let`s and the buffer's size are in [internals](internals.md#frame-records-and-the-frame-base).

## Native code cells

Both the interpreter and native code need to answer the same question before calling a function: has it been compiled, and if so, where is the code? The answer is kept in one place per supercombinator, its *native code cell*.

**What a cell is.** The mutable part of a supercombinator, 32 bytes: a function pointer, a counter, a request state and a verdict. The pointer is null until the supercombinator is compiled, and after that it holds the address of its `UnisonNativeFn`. The counter counts the times the interpreter ran the code itself because the pointer was null; it is how a function gets hot. The request state is how a definition is asked for exactly once: the interpreter moves it from "not requested" to "queued" when it asks for the supercombinator to be compiled, and the compile thread moves it to "taken" when it puts it in a batch. The verdict is the compile thread's decision on whether the supercombinator is worth compiling, and whether the interpreter should enter its code (see [What gets compiled](#what-gets-compiled-and-when)).

**Where cells live.** Outside the Haskell heap, so they never move and the GC ignores them. Each supercombinator's `GCombInfo` holds the address of its cell, next to its arity and frame size. Cells are allocated when code is loaded, every supercombinator gets one whether or not it's ever compiled, and they are never freed. A cell's address is only meaningful within one process and is not part of the serialized form of code.

**Who reads a cell, and how they find it:**

| Reader | How it finds the cell |
| --- | --- |
| The interpreter, at a `Call` or `App` | from the `GCombInfo` of the supercombinator it's about to run, which it already has in hand |
| Native code calling a known function | the cell's address is a constant in the generated code |
| Native code [calling a function value](#calling-function-values) | from the `GCombInfo` inside the closure |

In each case it's one load and a test for null. If the cell is null, the interpreter interprets the function, and native code exits to the interpreter. Calls within a batch don't use cells: the callee is in the same LLVM module, so the call refers to it directly, and LLVM can inline it. Installing compiled code is one pointer write: readers on other threads see either null or the function pointer, and both are valid, so callers never need recompiling when a callee is compiled later.

The cell's layout, its accessors, how it is attached during resolution and how generated code gets its address are in [internals](internals.md#the-interface).

## The trampoline

Native code never calls into Haskell, and the interpreter never calls native code directly from the middle of its own logic. Every transition in either direction goes through one piece of Haskell code, the *trampoline*. It's called that because control bounces off it: native code returns to the trampoline, which decides what runs next, which may be more native code.

**When the interpreter hands off.** The interpreter looks for native code in the two places where it would otherwise start interpreting:

- **When calling a function:** `Call`, and `App` whose target is a known supercombinator. It checks the supercombinator's [native code cell](#native-code-cells) for a main entry point.
- **When returning into a function:** `yield`, when it pops a `Push` frame. It checks whether that `Let` has a [re-entry point](#re-entry-points). This is how execution gets back into native code after an exit.

If it finds native code, the interpreter sets up the call exactly as it would for an interpreted one (arguments moved into place, stack grown if needed) and then passes the function pointer to the trampoline instead of interpreting the body.

**What the trampoline does:** fill in `Ctx` (the current addresses of the stacks and the constant pool, which the GC may have moved since the last entry, and the limits native code checks against); call the native function with an `unsafe` foreign call; read the stack pointers and any frame records back out of `Ctx`; build `K` frames from the records on top of the `K` it was entered with; then act on the status:

| `STATUS` | What the trampoline does next |
| --- | --- |
| `OK` | The function finished and its result is on the stack. Hand the result to the continuation with `yield`, as when an interpreted function returns. |
| exit index of a `Resume` | Continue interpreting at the recorded section. |
| exit index of a `Reenter` | Nothing, beyond being back in Haskell, where the runtime can switch threads or run a GC. Then enter the same function again. |
| exit index of a `GrowStack` | Grow the Unison stack with the interpreter's `ensure`, then enter the same function again. |
| exit index of a `CallOut` | Run the one recorded instruction with the interpreter's `exec`. Then enter the re-entry function, so the rest of the function runs natively. If the instruction raised an exception, don't re-enter: route it to the current exception handler, as the interpreter does for that instruction (the handler's continuation is the re-entry function, so the rest of the function is still native afterwards). |
| `EXIT_ERROR` | Read the error details from `Ctx` and throw the Haskell exception the interpreter would have thrown. |

**Properties worth noting.**

- **Neither stack grows across transitions.** By the time native code returns to the trampoline, all its C stack frames are gone. On the Haskell side, the trampoline's moves (`yield`, continue interpreting, re-enter native code) are all tail calls. So a program can go back and forth between native and interpreted code indefinitely.
- **The interpreter and native code interleave freely.** An interpreted function can call a native one, which can exit to the interpreter for an unknown call, whose callee may itself be native. Each side only ever sees the shared state: the Unison stack and `K`.
- **Nothing else in the interpreter changes.** Besides the checks above, the interpreter runs as it does today. When it resumes after an exit it can't tell that native code was involved, because of [the core rule](#the-core-rule).
- **Cost of a transition.** Entering native code costs an `unsafe` foreign call plus a few stores into `Ctx`, and a round trip allocates nothing. That's cheap, but not free, which is why native-to-native calls go direct and don't pass through the trampoline.

## What gets compiled, and when

The compilation unit is a batch of supercombinators, compiled as a single LLVM module. It is highly beneficial to compile multiple related supercombinators together, so that inlining and further optimizations can be applied that cross function boundaries. We use the following scheme:

* Each supercombinator yet to be native-compiled (an "interpreted" function) keeps a count of how many times it has been called, in its cell.
* When this count reaches a threshold (N, default 100), it is said to be "hot" and the definition is queued (idempotently) for a background compile thread. This thread submits batches of definitions for compilation.
* We use the call graph of the code to pull related definitions into the same batch. The compile thread grows the batch from the hot definition one neighbour at a time, using Prim's algorithm over the call graph in both directions (callers and callees), taking next the candidate with the most estimated calls into the batch, as long as that estimate, divided by the candidate's size relative to a typical definition, reaches a gate (N/4), up to a batch of size B. (The division keeps a large definition that runs once, such as a program's main body with many call sites into hot code, from joining the batch of everything it calls: compiling it would cost most of the module's time and save a handful of calls.) The estimate for an edge is the call count of the combinator holding the call site (every site is taken to run once per call of its combinator; a site in a local loop counts the loop's own calls), so a callee can join on the strength of its caller's count before it has been called at all, and a caller joins by its own count. The dependents direction tends to matter more since a callee is called at least as often as its caller, so it usually gets hot first. A definition whose own request is already waiting in the queue for compilation always joins the batch as well.
* Once compiled, pointers to native code for each supercombinator are installed while the program runs, one pointer write per supercombinator. Nothing assumes a batch appears all at once.

**Private copies.** A small function that is already compiled can be compiled again and included as a private copy in a later batch that calls it, so that the call is direct and LLVM can inline it; the original keeps serving every other caller. This adds some code duplication but allows for more inlining. The criterion is the callee's estimated work per call, not its size.

**Re-entry functions on demand.** One tricky part of the JIT is producing re-entry functions. Whenever native code exits for a call-out midway through a function, deferring to the interpreter, we want the continuation from that point in the function to also (eventually) get a native definition. Those on the slow path of an instruction that has a native fast path are generated on demand, when the interpreter has found their cell empty often enough; they are compiled in batches, or when requests for compilation have paused briefly.

**What isn't compiled.** Native code saves the interpreter's overhead, a few nanoseconds per instruction; an exit and the way back cost several times that. A function whose body is one foreign call would be slower compiled. So before a function is compiled, its MCode is walked to estimate, for an average path from entry to return, the overhead native code would save and the number of exits it is *certain* to take (instructions with no native version, calls to handlers, and calls to functions that are themselves not worth compiling). If the exits, weighted by the cost of a round trip, exceed the saving, the function is left to the interpreter and its cell records the verdict. Callees that haven't been judged are judged on the way, so the answer doesn't depend on the order in which functions get hot, and "not worth compiling" spreads to callers that do little but call such functions. Exits that only happen on a slow path (the poll, stack growth, a fast path's miss) don't count. What the rule can't see is the behaviour of function values: a loop that calls a closure which turns out not to be native takes an exit per iteration.

The same estimate gives a third verdict, *too small to enter*: a function whose average path saves less than entering native code from the interpreter costs, and that has no loop. It is compiled, and native callers call it directly, but when the interpreter is the caller it interprets the function as if it had no code.

The weights, the costs and the batch parameters are in [internals](internals.md#the-compile-driver).

**Later: specialization.** A call to `foldLeft` with `(Nat.+)` might produce a specialized version of `foldLeft`, with `(Nat.+)` effectively inlined into the body; a hot `List.map f` can get its own copy with `f` inlined, which removes the indirect call altogether.

## Calls

### Calling function values

Higher-order code is everywhere in Unison, so native code has to be able to call a function it was passed. In `List.map f xs`, the call to `f` is an `App` whose target is a value on the stack, not a supercombinator known at compile time. If each such call meant an exit to the interpreter, `List.map` would exit once per element.

**What a function value is.** For an ordinary function or lambda, it's a `GPAp` closure, which holds:

| Field | Meaning |
| --- | --- |
| `CombIx` | which supercombinator it refers to |
| `GCombInfo` | that supercombinator's arity, frame size, code, and the address of its [native code cell](#native-code-cells) |
| captured arguments | values already supplied, such as a lambda's free variables |

**What native code does.**

1. Check that the value is a `GPAp`. This is a test on the pointer's tag bits.
2. Read the arity and the number of captured arguments.
3. If captured plus supplied equals the arity, the call is exactly saturated. Copy the supplied arguments onto the Unison stack, then the captured ones above them, which is the layout the interpreter's `apply` produces.
4. Read the address of the native code cell from the closure's `GCombInfo`, and load the function pointer from the cell.
5. Call it like any other native function: a tail call if the `App` is in tail position, and otherwise a call followed by the status check.

Compared to a call to a known function, this adds a few loads and compares.

The interpreter's side of the same thing: when the interpreter applies a function value itself (`apply` on a `GPAp`), it checks the callee's cell just as `enter` does for a known callee, so a compiled function is entered natively however it was reached.

**A known combinator used as a value** (`App (Env f)` with no arguments, which is how a lambda becomes a closure) is a `GPAp` with nothing captured: a constant, kept in the [pool](#appendix-the-constant-pool) and built once.

**What still exits to the interpreter.**

| Case | Why |
| --- | --- |
| The callee isn't compiled yet | there's nothing to call; it will be compiled once it's hot |
| Too many arguments | the extra ones stay pending and are applied to the result, which needs more machinery |
| Too few arguments | the result is a new `GPAp`, built by a C helper from the function's closure in the pool |
| The value isn't a `GPAp`, for example a captured continuation | rare |

These exits work as described in the [exits table](#exits-how-native-code-covers-a-subset-of-the-language): in a `Let` binding, write a frame record and `Resume` at the binding; in tail position, `Resume` at the `App`.

### Tail calls

Loops in Unison are tail calls, so native code must run them without using any C stack.

- **A self tail call** is compiled as a branch back to the top of the function. The loop state is in the argument slots (or registers), so this is an ordinary native loop.
- **A tail call to another function** uses LLVM's `musttail`, which guarantees the call reuses the caller's C stack frame. Under the C calling convention `musttail` requires the caller and callee to have the same signature, which the uniform `UnisonNativeFn` type provides. Workers have differing signatures, so they use LLVM's `tailcc` convention, which guarantees tail calls without that restriction.
- **A tail call returns the callee's `STATUS` directly** to the caller's caller, so there's no status check and no `K` frame to record.

Only non-tail calls use C stack, and those are covered by the [stack guards](#stack-growth-and-preemption).

### Workers: arguments and results in registers

The uniform signature passes everything through the Unison stack: the caller stores the arguments, the callee loads them, and the result comes back the same way. That is right for the trampoline and for calls through a cell, but between functions compiled together it is all overhead, and LLVM can't remove it, even when it inlines, because the stack is reachable from `Ctx`.

So a function is also compiled as a **worker**: an internal LLVM function `(ctx, fp, u1, b1, ..., un, bn) -> {status, u, b}` with LLVM's `tailcc` convention. The arguments are parameters, the result is two return values, and the Unison stack isn't written at all unless something exits. The function its cell points to becomes a **wrapper** with the uniform signature: load the arguments from the stack, call the worker, store the result. Calls between functions of one module go to the worker directly, and an inlined worker is plain SSA code in its caller. The frame keeps its place on the Unison stack: a worker is passed the frame pointer its frame would have, and if it exits it writes its slots there, so the interpreter sees the same stack either way.

Every Unison function returns one value, but the unit of compilation is an MCode combinator, and MCode's `Yield` can carry several (a primitive such as `LOAD` pushes two results, and a `Let` binding bound to several variables is a combinator that returns all of them to its body). Those keep the uniform form only; ordinary functions get a worker.

How a worker exits, tail calls between the two signatures, the fast entry for base cases and the cold branch weights are in [internals](internals.md#the-generator), with a worked example.

## Stack growth and preemption

Native code can't grow a stack or give up the CPU by itself. Both are handled the same way: native code checks a condition at known points, and if the check fires, it exits to the interpreter, which does the work in Haskell and then calls back into native code.

**The C stack, checked at function entry.** Unison programs can recurse deeply. Above a depth limit, exit instead of calling, and the rest runs from the interpreter with heap-allocated frames. The limit is the stack pointer compared with a limit in `Ctx`, set from the thread's stack bounds minus a reserve. Beyond the limit, recursion alternates between native and interpreted frames, one interpreted `Let` per limit hit.

**The Unison stack, checked once at function entry.** Every supercombinator has a maximum frame size that's known at compile time: the most Unison stack it can use, including the argument slots for any call it makes. Every native function starts by checking that a frame of that size fits in the space remaining, the check the interpreter does with `ensure` when it enters a supercombinator. If it fails, the function returns a `GrowStack` exit before doing anything else; its arguments are already in their stack slots, so there's nothing to write back. Once the entry check has passed, every slot the function could write, including those written on its exit paths, is inside the space that was checked. Checking at entry rather than at the call site is one check per function, not one per call, and works for calls to function values, where the caller doesn't know the callee's frame size. Only the interpreter reallocates the stack arrays, so their addresses stay valid for as long as native code runs.

**Preemption.** Haskell threads aren't preempted at arbitrary points: the runtime's timer sets a flag on the capability, and compiled Haskell code notices the flag the next time it allocates, then hands control to the scheduler. Native code inside an `unsafe` foreign call never reaches that check. Left alone, a long-running native loop would starve the other Haskell threads on its capability, stall the whole program whenever any thread needs a GC (a collection waits for every capability to stop), and delay `killThread` and timeouts. So native code polls at the entry of every native function, which includes the top of a loop, since a self tail call branches back there: Unison has no loops other than calls, so any code that runs for an unbounded time must keep entering functions, and everything between two function entries is straight-line code of bounded length. The poll checks the capability's context-switch request and the [allocation budget](#memory); when it fires the function returns a `Reenter` exit, and when the thread next runs the interpreter enters the same function again. It costs a load, a compare and a branch that's almost never taken, per function entry.

## Memory

Native code allocates real Haskell heap objects, in the same allocation block compiled Haskell uses. This is safe because GHC can't run a GC on this capability during an `unsafe` foreign call, and native code keeps no heap pointers once it returns. The constructor layouts are GHC's, so they are not hard-coded: they are learned by probing sample closures at startup, because `UNPACK` pragmas only take effect in optimized builds and the layout can differ between builds. Heap constants (`Reference`s, type tags, `GCombInfo`, literals) come from the [constant pool](#appendix-the-constant-pool), never from addresses baked into code.

**Pointer tagging.** Pointers to evaluated constructors carry the constructor number in their low bits. Native code must untag before reading fields and must store correctly tagged pointers when it writes into `bstk` or a constructor field. Every field of `GClosure` and `Val` is strict, so everything reachable from `bstk` is in weak head normal form and native code never has to evaluate anything; but a pointer taken from a stack slot may still be *untagged* (a top-level constant stored as itself), so native code checks the tag before reading through such a pointer and before storing it into a strict field, and leaves the rest to the interpreter.

**Write barrier.** GHC's collector is generational, and a minor GC doesn't scan old-generation objects unless it has been told they changed. `bstk` is a long-lived mutable array, so it's in the old generation, and a pointer stored into it without telling the GC can leave a new object looking unreachable. Native code stores into `bstk` directly, with no barrier, and pays the barrier once per return to Haskell rather than once per write: no GC can run during the `unsafe` call, so the marks only have to be in place by the time native code returns. Other mutable objects native code writes to (`MutVar`s, arrays) get the same barrier compiled Haskell gives them.

**The allocation budget.** `allocate` takes blocks from the nursery, and once the nursery is empty it takes fresh blocks from the block allocator without limit; GC is only triggered when a *Haskell* heap check fails, and nothing native code does triggers one. So native code keeps an allocation budget, checked by the [poll](#stack-growth-and-preemption): after a budget exit, the interpreter's own allocation hits a heap check within a block or so, and the GC runs. The budget is what bounds memory growth, not a courtesy.

How allocation is done (the inline bump pointer), the barrier's details and the layouts probed are in [internals](internals.md#allocation-and-the-barrier).

## Builtins and data structures

Most of what library code does is in builtins over a few representations: lists, text, bytes, numbers, arrays and refs. Two principles keep those native from end to end:

- **The representations are strict in every field** (a `List` is a strict finger tree, a `Text` and a `Bytes` are ropes of packed chunks on the same tree), so everything reachable from one is an evaluated constructor, and native code can take them apart and build new ones with no thunk in sight.
- **The native operations are complete ports of the Haskell ones**, as C helpers that generated code calls directly (the smallest generated inline; a few compiled from C to LLVM IR and compiled again by the JIT, so that what they return comes back the same way on every platform), and they are checked against the Haskell operations at startup on sample values (and by a longer randomized test on request); if anything differs, the JIT stays off. A helper that can't decide a case exactly answers "not handled" and the interpreter does it, so a helper is never wrong, only sometimes absent. The price is that a helper has to be kept in step with the Haskell operation it ports, which is what the checks are for.

The results are the interpreter's bit for bit: GHC's `Double` semantics for floats, the interpreter's bounds arithmetic for arrays, the Haskell lexer's forms for numbers read from text. Which builtins are native is in [builtins.md](builtins.md); the representations and helpers are in [internals](internals.md#data-representations-and-the-c-helpers).

## Alternatives considered

Things that were proposed along the way and not done, so that they aren't proposed again without the reason being known:

- **A dynamic safety net instead of the static estimate.** Count a function's exits against its entries at run time and drop its native code when it exits nearly every time. It can't tell a function that is all call-outs from one that does real work and then calls out once, and it compiles the code it later rejects. The static estimate reads the code, needs no bookkeeping, and never compiles what it refuses; what it can't see is behaviour (which branch runs, whether a function value is native), and the cheaper round trip made that blind spot cost little.
- **Re-entry points as one function with an entry-index switch.** Every `Let` boundary on the fast path would become a merge point, which hurts LLVM's optimization of the fast path, and the entry branch would be shared between normal calls and re-entries. Separate functions, generated when used, keep the fast path straight.
- **Static analysis of which re-entry points are reachable.** Nearly all are: any callee can exit at its poll, its stack check or the C stack guard, so the analysis would remove almost nothing. Whether a point is *used* is dynamic, so it is counted.
- **Native code on a C stack of its own,** so that a call-out switches stacks, lets the interpreter run one instruction, and switches back with every native frame still there. The GC can run during that instruction and move objects, so every parked frame would have to put its live values on the Unison stack before the call and reload them after, which is most of the cost of unwinding, plus a parked stack per Unison thread and per nested evaluation, and exceptions would have to discard it. Set aside in favour of making exits rarer and cheaper; worth revisiting only if exits still dominate.
- **A bare code pointer in a `CallOut` exit** instead of a cell. The re-entry function can itself exit with `GrowStack` or `Reenter`, both of which mean "call this function again", and the trampoline does that through a cell.

## Appendix: the constant pool

Native code needs some Haskell heap objects: the `Reference` written into every constructor by `Pack`, the type-tag closures for unboxed values, constructors without fields, `GCombInfo` for building partial applications, and `Lit` values. These objects move on every GC, so their addresses can't be baked into machine code. Instead there is a *constant pool*: a `MutableArray Closure` owned by Haskell, whose address is passed to native code on every entry, exactly like `bstk`, through `Ctx`. Generated code refers to constants by pool index. Addresses are stable for the duration of an `unsafe` call, so this is safe for the same reason reading `bstk` is.

There is one global pool rather than one per module, because native code in one module calls into another without passing through the trampoline, so there would be no moment to switch pools. Entries are interned by key when a module is compiled, so two modules using the same `Reference` share an index. How the pool grows while native code holds the old array is in [internals](internals.md#the-constant-pools-growth).
