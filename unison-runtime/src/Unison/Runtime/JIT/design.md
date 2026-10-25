# Unison JIT Design

Sep 26, 2026 · Paul Chiusano

This document describes the design of Unison's new LLVM-based JIT compiler, which can execute Unison code at native speeds. It is in two parts: [Part 1](#part-1-overview) is an overview of the major pieces, with links to its section of [Part 2](#part-2-details) for more depth. Haskell and C names link to their definitions in the code. Also:

* How to turn it on, what it needs and what the settings mean is in [usage.md](usage.md)
* How to build, run and test it is in [development.md](development.md)
* Which builtins are native is in [builtins.md](builtins.md)
* Ideas not done yet are in [optimization-ideas.md](optimization-ideas.md)
* Benchmarks from along the way in [benchmarks/](benchmarks/)

## Part 1: Overview

At a high level, here's how the JIT works:

* Functions run interpreted until they are called enough times (default 100), at which point they are considered hot and are scheduled for compilation on a background thread.
* The background thread compiles batches of related functions (by traversing the dependency graph of the code), then installs a pointer to each compiled function in a place the interpreter can see it. The next time the interpreter invokes each function, it notices there's a non-null pointer to native code and calls into that instead of using the interpreter.
* As discussed below, we have a scheme for allowing native code to exit back to the interpreter, which is useful for subsets of the language that have no native equivalent (they can just run interpreted) and for a few other purposes, too.

### The big idea of partial compilation

A bit of history to motivate this design: we previously attempted a JIT that aimed to compile the entirety of the language to Racket Scheme (itself compiled at runtime to native code). This effort was ultimately abandoned for a few reasons: it was quite complicated to try to cover literally everything in the Unison language, especially the dynamic code-loading and serialization support used by Unison Cloud. And other than simple arithmetic loops, we never saw major performance wins over the interpreter. Certain functional code was actually *faster* with our interpreter, which is pretty well tuned. It was also very much an "all or nothing" effort that couldn't be used until every Unison builtin had a Scheme implementation.

This new JIT uses a different approach that allows it to be useful immediately. The idea is to compile functions to native code with [LLVM ORC](https://llvm.org/docs/ORCv2.html). But rather than needing to cover the entirety of the Unison language and its builtins, we allow native-compiled functions to "exit" back to the interpreter. Thus, delimited continuations, abilities, and any other subset of the language which is hard to compile to LLVM will continue to work but will defer to the interpreter at runtime. The idea is similar to what's done in a tracing JIT, where branches "off trace" revert to the interpreter, but here we're applying the idea to a function-at-a-time JIT. See [further reading and references](#futher-reading-and-references) for related work.

### Goals

- No change to language semantics or serialized code.
- Large speedups on most Unison code, especially straight line code that doesn't use fancy features like abilities or delimited continuations.
- Support only a subset of the Unison language, and grow it over time. Anything unsupported runs in the interpreter, with identical behaviour.
- Fast support for affine handlers, see [Ability requests](#ability-requests).

### Non-goals, for now

- Compiling use of delimited continuations natively. However, requests of affine handlers, and the handlers' cases, are native, since these don't require any delimited continuation fanciness. 
- Making compiled code callable as an ordinary Haskell function, or making it follow GHC's calling conventions.
- Compatibility with the Racket / `run.native` backend. This backend is defunct.
- Caching compiled code on disk across runs. Although we could probably get a lot of mileage out of this (compiled definitions keyed by hash never need rebuilding), if compilation is fast enough, this may never be worth it.
- GHC's non-moving collector (`+RTS -xn`). Its write barrier is of another kind: it records the pointer being *overwritten* at every store into an old object, so native code would need a call per store into `bstk`, a `Ref` or an array where it now marks once per return ([The write barrier](#the-write-barrier)). Not hard, but a cost on every store, and nothing checks for the collector at startup yet, so the JIT must not be run with it.
- CPUs other than arm64 and x86-64. The JIT runs on macOS, Linux and Windows on those two (CI tests all five combinations, see [usage.md](usage.md#what-it-needs)); nothing the generator emits is per platform, and the little that is per platform is C (finding LLVM, the thread's stack bounds) or a calling-convention choice (Windows).

### Tricky concerns

A few things to be aware of throughout the design:

* Haskell has a moving garbage collector, so native code should never retain heap pointers after returning control back to the interpreter. Otherwise those pointers could get relocated and the native code would be left with a stale pointer. See [Memory](#memory) and [the constant pool](#the-constant-pool).
* Native code must be preemptible in the same way as other Haskell functions. See [Preemption](#preemption).
* Native code should keep the same conceptual model of the stack being growable. We do not want perfectly cromulent functional programs to crash because of a randomly low / fixed C stack limit. See [Stack growth](#stack-growth-and-preemption).

### The pieces

The JIT integrates tightly with the existing interpreter and shares state with it: the Unison stack and the continuation. Here are the relevant parts: 

- **[The Unison stack and MCode trees](#the-unison-stack-and-mcode).** The interpreter runs *supercombinators* whose bodies are [`MCode`](../MCode.hs) instruction trees, on a [`Stack`](../Stack.hs#:~:text=data%20Stack) of two arrays (unboxed words and boxed closures) with a heap-allocated continuation [`K`](../Stack.hs#:~:text=data%20K). The JIT compiles the same `MCode`, in dynamically formed batches of supercombinators, to functions that operate on the same stack.
- **[Exits](#exits-how-native-code-covers-a-subset-of-the-language).** Every compiled function has one C signature. It returns `OK` when it finishes, or an *exit*: an index into a table of [`Exit`](Exits.hs#:~:text=data%20Exit) specifying what the interpreter should do next (resume at a section, run one instruction and re-enter, grow the stack, or just come back later). Native code follows [the core rule](#the-core-rule): before giving control back to Haskell, it ensures the stack is exactly what the interpreter would have produced. Native code can and does use more efficient ways of passing data around, it doesn't have to read and write to the Unison stack everywhere, but when unwinding before an exit back to the interpreter, it makes sure the Unison stack is as expected by the interpreter. 
- **[Re-entry points](#re-entry-points).** After an exit in the middle of a function, the rest of the function has its own native entry, so one unsupported instruction doesn't make the whole remainder run interpreted.
- **[Frames](#frames-native-code-records-haskell-builds).** Native code calls native code through the C stack and never builds up Unison continuations directly. Instead, when an exit unwinds through native callers, each writes a three-word *frame record*, and the interpreter builds the [`Push`](../Stack.hs#:~:text=data%20K) frames from them that construct the right `K` continuation. This avoids native code needing to know details of how the Haskell side represents continuations.
- **[Native code cells](#native-code-cells).** Each supercombinator has a 32-byte cell outside the Haskell heap holding its code pointer, a call counter, a request state and a verdict that says if the function is eligible for compilation (see below). Both the interpreter and native code find compiled code by loading from this cell.
- **[The trampoline](#the-trampoline).** Every transition between the interpreter and native code goes through one piece of Haskell, the trampoline. It fills in the per-thread [`Ctx`](../../../../cbits/jit_rt.h#:~:text=typedef%20struct), makes an `unsafe` foreign call to native code, reads the results back and acts on the status. Neither the C stack nor the interpreter call stack grows across transitions, so the two can interleave indefinitely.
- **[What gets compiled, and when](#what-gets-compiled-and-when).** A function that has been called a threshold number of times is *hot*; a background thread compiles it in a batch with its neighbours in the call graph, as one LLVM module, and installs the code one pointer write at a time. Because there's some overhead to exiting native code, functions estimated to involve a lot of back and forth between the interpreter and native code are kept interpreted.
- **[Code generation](#code-generation).** Each supercombinator's MCode is translated to LLVM IR. Stack slots become registers, exit paths write back only the live slots through one runtime call, and each function is compiled as a *worker* that passes arguments and results in registers, with a *wrapper* in the uniform signature for the interpreter and a *fast entry* for base cases.
- **[Calls](#calls).** Known callees are called directly (within a module) or through their cell; function values are called by reading the cell out of the [`GPAp`](../Stack.hs#:~:text=%7C%20GPAp) closure; tail calls use LLVM's `musttail`.
- **[Stack growth and preemption](#stack-growth-and-preemption).** Native code checks the Unison stack, the C stack and the runtime's context-switch flag at known points, and exits when a check fires; the interpreter does the work and calls the function again.
- **[Memory](#memory).** Native code **allocates ordinary Haskell heap objects** with a bump pointer, respects pointer tagging and the generational write barrier, and keeps an allocation budget so that GC still happens. Layouts of Haskell runtime objects are probed at startup, not hard-coded.
- **[Builtins and data structures](#builtins-and-data-structures).** Lists, text, bytes, arrays, refs, numbers, hashing, comparison and records are handled natively by C helpers that are ports of the Haskell operations, checked for consistency against the interpreted equivalents at startup. Some of the interpreter's optimized replacements are switched off under the JIT because native Unison code beats exiting to the interpreter.
- **[Ability requests](#ability-requests).** Requests of affine handlers run natively (including the handlers' pattern matching on the request); the handler environment is added to `Ctx` which is passed to native code. As most handlers are affine, Unison code that uses abilities often sees a significant speed boost.
- **[The constant pool](#the-constant-pool).** Heap objects native code needs (references, type tags, literals) live in one array per process whose address is passed in on every entry, since they move on every GC.

## Part 2: Details

### The Unison stack and `MCode`

Before native compilation, our existing machinery has already converted the code to a set of _supercombinators_ (or sometimes just "functions" in this document). A supercombinator is a function definition in [A-normal form](https://en.wikipedia.org/wiki/A-normal_form) that contains no inner functions or lambdas and which can only call other supercombinators. The body of a supercombinator is a value of type `MCode` (for "machine code"). The JIT compiles these bodies and shares the interpreter's [stack](#the-stack), [values](#values) and [continuation](#the-continuation). The relevant types:

- [MCode](#mcode): the instruction trees the interpreter runs and the JIT compiles.
- [The stack](#the-stack): two arrays and three pointers.
- [Values](#values): the closure constructors and the unboxed representation.
- [The continuation](#the-continuation): the heap-allocated frames that say what happens after a return.

#### MCode

[`Unison.Runtime.MCode`](../MCode.hs). Each supercombinator is a [`GSection`](../MCode.hs#:~:text=data%20GSection) tree of instructions. Instructions include things like [`DMatch`](../MCode.hs#:~:text=data%20GSection) (pattern matching on a value), [`App`](../MCode.hs#:~:text=data%20GSection) (call an unknown function), [`Call`](../MCode.hs#:~:text=data%20GSection) (fully saturated call to known function), [`ForeignCall`](../MCode.hs#:~:text=data%20GInstr) (call a foreign function), [`Let`](../MCode.hs#:~:text=data%20GSection) (evaluate and push result to stack), and so on. The instructions that sit between sections are [`GInstr`](../MCode.hs#:~:text=data%20GInstr). Some builtin functions like arithmetic have direct instructions (via [`Prim1`](../MCode.hs#:~:text=data%20Prim1) and [`Prim2`](../MCode.hs#:~:text=data%20Prim2)) but most others are `ForeignCall` instructions that run a Haskell function and push results to the stack. A supercombinator's arity, frame size, entry section and [native code cell](#native-code-cells) are its [`GCombInfo`](../MCode.hs#:~:text=data%20GCombInfo).

Supercombinators exist in two forms: *unresolved*, where references to other supercombinators are [`CombIx`](../MCode.hs#:~:text=data%20CombIx) names (what the compiler emits and what is read from a compiled program file), and *resolved*, where those references point to the supercombinators themselves (what the interpreter runs and what closures contain). Only resolved supercombinators can run.

#### The stack

[`Unison.Runtime.Stack`](../Stack.hs). The [`Stack`](../Stack.hs#:~:text=data%20Stack) is two arrays, an unboxed `MutableByteArray` (`ustk`) of words and a boxed `MutableArray Closure` (`bstk`) of closures, where the same index in each is one *slot* holding one value ([Values](#values)), and three indices into them:

- `sp`, the stack pointer: the last slot in use. Instructions name slots relative to it ([`peekOff`](../Stack.hs#:~:text=peekOff%20%3A%3A)` stk i` reads slot `sp - i`), and since the emitter knows how many slots each instruction pushes, every slot an instruction names is a fixed offset from the start of its function.
- `fp`, the frame pointer: the slot just below the running function's *frame*, the slots `fp + 1` to `sp` that belong to it. A function of arity n is entered with its arguments in `fp + 1` to `fp + n`, and everything it pushes goes above them.
- `ap`, the arg pointer: the slot just below any *pending arguments*, which sit in `ap + 1` to `fp`. Usually `ap = fp`. When a function is applied to more arguments than its arity, the extra ones wait there, below the frame, and are applied to the function's result when it returns.

For example:

```
f n = 
  x = g n
  x + 10
```

This is entered with `n`, `f` has `n` in slot `fp + 1`, and `sp = fp + 1`. To call `g` it copies `n` into the next slot as `g`'s argument, and the `Let` saves the frame's size (`sp - fp`, here 1) and pending-argument count (`fp - ap`, here 0), `g` then runs with its own `fp` just below its argument, above everything of `f`'s. When `g` returns, its result is in the slot right above `f`'s frame, [`restoreFrame`](../Stack.hs#:~:text=restoreFrame%20%3A%3A) subtracts the two saved sizes to get `f`'s `fp` and `ap` back, and `x + 10` runs with `x` on top, at `sp`. 

Other notes on the stack:

* Both arrays are unpinned and are reallocated when the stack grows ([`ensure`](../Stack.hs#:~:text=ensure%20%3A%3A)).
* Both arrays keep a few spare elements past the last slot, an *out-area* ([`nativeOutWords`](../Stack.hs#:~:text=nativeOutWords%20%3A%3A), [`nativeOutPtrs`](../Stack.hs#:~:text=nativeOutPtrs%20%3A%3A)), through which native code hands its results back to the [trampoline](#the-trampoline) without allocating.

#### Values

[`Closure`](../Stack.hs#:~:text=newtype%20Closure) wraps [`GClosure`](../Stack.hs#:~:text=data%20GClosure): `GList`, `GText`, `GEnum`, `GData1`, `GData2`, `GData5`, `GData3`, `GData4`, `GData6`, `GDataG`, [`GBytes`](../Stack.hs#:~:text=%7C%20GBytes), [`GPAp`](../Stack.hs#:~:text=%7C%20GPAp), [`GCaptured`](../Stack.hs#:~:text=%7C%20GCaptured), `GRecord`, [`GForeign`](../Stack.hs#:~:text=%7C%20GForeign) (maps, arrays, refs and the other Haskell values, the [`Foreign`](../Stack.hs#:~:text=data%20Foreign) wrappers), `GUnboxedTypeTag`, `GAffine`, `GBlackHole`. The first six have their own pointer tags, which is why the order is what it is ([Layouts](#layouts)). A stack slot's value is a [`Val`](../Stack.hs#:~:text=data%20Val): unboxed values are a `ustk` word plus a type-tag closure (such as [`natTypeTag`](../Stack.hs#:~:text=natTypeTag%20%3A%3A)) in the matching `bstk` slot; boxed values are the closure, with the word unused.

#### The continuation

[`K`](../Stack.hs#:~:text=data%20K) is the interpreter's continuation: what to do after the running function returns. It is a linked list of *frames* on the Haskell heap, which the interpreter builds as it runs. 

Looking again at this example:

```
f n = 
  x = g n
  x + 10
``` 

Before calling `g`, the interpreter pushes a `Push` frame on `K` holding what it needs to come back to `f`: the frame's size and pending-argument count (to restore `f`'s frame, as above), where to resume (the section `x + 10`, as a [`CombIx`](../MCode.hs#:~:text=data%20CombIx) plus an [`RSection`](../MCode.hs#:~:text=type%20RSection)), and the [native code cell](#native-code-cells) of that `Let` body, since the body is where native code may take over again. Thus, the interpreter is eagerly constructing the continuation for possible capture by any code in non-tail position. (In contrast, native code lazily constructs continuations only when needed.)

When `g` returns, [`yield`](../Machine.hs#:~:text=yield%20%3A%3A) pops the `Push`, restores `f`'s frame and continues at `x + 10`.

The other kinds of `K` frames come from abilities: a `handle` installs a prompt, `Mark` (`AMark` for an affine handler); a request to a handler that isn't affine is a `Capture`, which copies the stack segments and the `K` frames above the prompt into a continuation value; `Local` records a handler environment to switch back to, and `Keep` and `CB` are bookkeeping of the same machinery. What each frame means for native code is in [Frames](#frames-native-code-records-haskell-builds).

### Exits: how native code covers a subset of the language

Native code, once compiled, is entered through a foreign call, using the C calling convention. It allocates Haskell heap objects just like any other compiled Haskell code. For instructions that can't be run in native code, the function returns in such a way that the interpreter knows to run that instruction and then re-enter native code right after. This is called a "call-out"; see [re-entry points](#re-entry-points). In more detail:

- [The core rule](#the-core-rule): the invariant that makes exits correct, and its consequences.
- [The native signature](#the-native-signature): every compiled function is called the same way, through `Ctx`.
- [Status values and the exits table](#status-values-and-the-exits-table): what a return value means, and the `Exit` type.
- [When native code exits](#when-native-code-exits): the table of situations and the kind of exit each takes.
- [Write-back](#write-back): how an exit puts the frame back on the Unison stack.

#### The core rule

Whenever native code hands control back to Haskell, the Unison stack (`ustk`, `bstk`, `ap`, `fp`, `sp`) will be exactly as the interpreter would have left it at that `MCode` point. Native code may do other stuff internally, keeping values in registers, using a different representation of the call stack, etc, but must produce the correct Unison stack before returning.

This has some consequences:

- Native-compiled supercombinators that have to exit back to the interpreter will need to write to the stack and construct `K` frames on demand. At the time of compilation, we have enough information to be able to construct code that will do this for each possible exit point of the function. (Each continuation point is just an `MCode` subtree that's easily available because of the A-normal form nature of the code.)
- For native-to-native calls, the caller will check whether the callee is returning normally (because it completed) or if it is exiting back to the interpreter partway through the function. If exiting back to the interpreter, the caller will record what's needed to build its own `K` frame (see [Frames](#frames-native-code-records-haskell-builds)) and then itself exit.
- `Capture` (for capturing a delimited continuation) won't ever be run in native code. Instead, at every point where we would capture, the native code will be exiting back to the interpreter, and producing ordinary stack segments and `K` frames as it unwinds. Thus, we don't need LLVM code to have a concept of continuations at all, it's just another unsupported language feature that exits to the interpreter whenever it's hit.

#### The native signature

Every native-compiled function has a wrapper with the same signature. This uniform signature is what the interpreter calls ([`UnisonNativeFn`](../../../../cbits/jit_rt.c#:~:text=typedef%20int64_t) in C, [`NativeFn`](Native.hs#:~:text=data%20NativeFn) in Haskell):

```c
typedef STATUS (*UnisonNativeFn)(Ctx *ctx, int64_t ap, int64_t fp, int64_t sp);
```

(Each supercombinator also gets compiled as a separate *worker* that takes arguments as ordinary parameters and returns results via unboxed LLVM tuples. The wrapper calls the worker, and within a batch, workers can call each other and themselves directly without having to stash values on the Unison stack. See [Workers](#workers-wrappers-and-the-fast-entry).)

[`Ctx`](../../../../cbits/jit_rt.h#:~:text=typedef%20struct) is a per-thread value: one `malloc`ed struct per OS thread, reached through a thread-local variable ([`get_ctx`](../../../../cbits/jit_rt.c#:~:text=static%20UnisonJitCtx%20%2Aget_ctx)). Per OS thread is enough, although it is Haskell threads that run Unison code, because a `Ctx` is in use only for the duration of one `unsafe` foreign call, during which the OS thread runs nothing else, and the trampoline fills it in afresh at every entry, so a Haskell thread that is descheduled between two entries leaves nothing in it. It holds what native code needs from the runtime (the addresses of the Unison stacks and of the constant pool, the current handler environment, current allocation block limits, etc) and is also where native code writes info the interpreter needs on exit (since a C function can only return the one `STATUS`). The Haskell side mirrors its layout in [`CtxOffsets`](Codegen.hs#:~:text=data%20CtxOffsets) and checks it at startup against [`unison_jit_ctx_layout`](../../../../cbits/jit_rt.c#:~:text=int64_t%20unison_jit_ctx_layout), so the two can't disagree silently.

#### Status values and the exits table

`STATUS` is an `int64_t`. [`OK = 0`](../../../../cbits/jit_rt.h#:~:text=%23define%20UNISON_JIT_OK) means the function returned normally. Positive values are an exit, a 1-based index into the exits table. Negative values are reserved: [`EXIT_ERROR = -1`](../../../../cbits/jit_rt.h#:~:text=%23define%20UNISON_JIT_EXIT_ERROR) is an internal failure the trampoline turns into an error (generated code never returns it; a Unison-level error is a [resume](#when-native-code-exits), and the interpreter raises it), and -2 is a worker's "tail call this for me" ([Workers](#workers-wrappers-and-the-fast-entry)), which never reaches the trampoline.

The exits table is a growable array that tells the interpreter what to do when native code returns an exit. It is an ordinary Haskell structure, since an `Exit` holds Haskell heap objects (the tree of instructions to run on resumption, for instance). Native code never reads an `Exit` directly: it only returns an index into the exits table, as a constant baked into the generated code. (Each supercombinator has a fixed number of possible exits, known at compilation time, so it's no problem to give each supercombinator a range in the process's exits table.)

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

(The real type, [`Exit` in `Exits.hs`](Exits.hs#:~:text=data%20Exit), also records the combinator each exit belongs to, for statistics. The exits name [native code cells](#native-code-cells) rather than code pointers because code is installed while the program runs: an exit can be taken before its continuation's cell has been written, and a call-out's re-entry function may not have been generated at all yet.)

The exits table and the [frame table](#frame-records) are arrays of Haskell values that grow by copying ([`registerExits`](Exits.hs#:~:text=registerExits%20%3A%3A), [`lookupExit`](Exits.hs#:~:text=lookupExit%20%3A%3A)). Looking one up is on the path of every exit, so it is two loads. Only the compile thread writes: an entry is in place before the code that returns its index is installed, and a reader that still holds an older array finds every index it can be handed there.

Notes on the `Exit` type:

* `Resume` is straightforward, it's just interpreted code to run on resume.
* `CallOut` is needed so that a single exit in the middle of a function doesn't cause the whole rest of the function to run interpreted: the interpreter runs the one instruction and then resumes using the native re-entry function (if its cell is non-null) or the given `RSection` (if not).

#### When native code exits

Here are different situations where native code can exit back to the interpreter:

| Situation | Exit kind |
| --- | --- |
| Any instruction without a native version | `CallOut`, re-enter after the instruction |
| An instruction with a native fast path whose slow case is met (a builtin helper answers "not handled", a bounds check fails, a pointer turns out untagged) | call-out at the instruction, re-enter after it; the re-entry function lazily generated if hit enough times |
| Unsupported control *section* (`Jump`, `Die`, …), or `Capture` / `Discard` / `Reset` | `Resume` at that section |
| An ability request whose handler isn't affine ([Ability requests](#ability-requests)) | `Resume` at the section |
| A call native code can't make, in a `Let` binding: the callee isn't compiled, or it's an `App` of a function value in one of the [cases left to the interpreter](#calling-function-values) | record the `Push` frame for the `Let` body, `Resume` at the binding. Native code is re-entered at the `Let` body when the callee returns. |
| Unison stack needs to grow | `GrowStack`: the function exits at its entry check, before doing anything else. The interpreter grows the stack and calls the same function again. See [Stack growth and preemption](#stack-growth-and-preemption). |
| Preemption poll, or allocation budget used up | `Reenter`: the function exits at its entry poll. Once the Haskell runtime has done what it needed to, the interpreter calls the same function again. |
| Function finishes successfully | not an exit: status `OK`, which goes to [`yield`](../Machine.hs#:~:text=yield%20%3A%3A) |
| A *handler environment switch* native code can't make (below) | `CallOut`, re-enter after the instruction |
| Any exit from inside a handler case whose environment switch native code made (below) | that exit, plus a `Local` frame record |

**Handler environment switches.** An affine handler's case must run with the handlers that were in force where the handler was installed, not the requester's, so on entering the case the handler environment is switched, and at the end of the case switched back; the interpreter does this with the `InLocal` instruction and a `Local` frame on `K` that remembers the old environment. Native code makes the switch itself ([the switch](#the-switch)), except when the value isn't an affine handler's closure or 32 switches are already nested, the most an exit can hand back; then the interpreter makes it. And if native code exits for any reason from inside a case it switched, it is the interpreter that will finish the case, so the exiting function also writes a frame record carrying the old environment, which the interpreter rebuilds as the `Local` frame that switches back at the end.

Runtime errors need no machinery of their own, because none are raised while native code runs. A `Die` section, which is what `bug` and a missing match case compile to, is just a `Resume`: the interpreter runs it and raises. A builtin that would fail (an index out of bounds, an I/O error) fails on the interpreter's side too: the native fast path of the instruction checks for exactly the cases the interpreter rejects and takes the call-out for them, so the interpreter runs the instruction and raises the exception. This matters because a Haskell exception can't unwind through C frames, so nothing may ever throw while native code is on the C stack.

#### Write-back

Before it returns an exit, native code puts the frame back on the Unison stack as the interpreter expects it, and puts the new stack pointers into `Ctx` for the trampoline to read. The pieces:

- **Only the live slots are written.** [`liveAt`](Codegen.hs#:~:text=liveAt%20%3A%3A) walks the code the exit resumes at and collects the slots it reads; the rest of the frame is left as it was, which is whatever an earlier frame wrote there, valid closures. A slot that is dead at the exit stays dead afterwards, so nothing can later read one that wasn't written.
- **One call per exit site** ([`joinCall`](Codegen.hs#:~:text=joinCall%20%3A%3A)). The site stores the live slots into an argument block (one `alloca` per function) and calls [`unison_jit_exit_frame`](../../../../cbits/jit_exit.c#:~:text=void%20unison_jit_exit_frame) with a constant *descriptor* saying where the slots go, whether to record the stack pointers, and which [frame records](#frame-records) to append; the routine does the stores, and the site returns the status itself. Generating the stores at every site instead made the exit paths most of a function's code ([2026-10-09 write-back](benchmarks/2026-10-09-write-back.md), [exit call](benchmarks/2026-10-09-exit-call.md)); `UNISON_JIT_EXITS=blocks` keeps that form for comparison.
- **The routine is IR, not C in the runtime.** It uses LLVM's `preserve_most` calling convention, so the call clobbers almost no registers on the path that matters, where the exit isn't taken. Only clang spells that convention in C, so like [the IR helpers](#the-ir-helpers) it is compiled to IR at development time ([`jit_exit.c`](../../../../cbits/jit_exit.c) to `jit_exit.ll`) and compiled by the JIT at startup.

### Re-entry points

A compiled function can be interrupted partway through its execution in three ways:

1. A call-out, where the interpreter runs one instruction and native code should continue after it
2. A non-tail call whose callee exits, where the interpreter finishes the callee's work and native code should continue with the `Let` body. 
3. An ad-hoc interruption, due to running out of stack, reaching the end of the current allocation block, or being pre-empted. These conditions are checked at at function's entry, and if hit, they re-enter the same function from the start.

We're going to focus on the first two items, since they require resuming *in the middle of a function* whereas ad hoc interruptions resume once at its start.

Let's look at some code:

```
f n =
  x = g n      -- non-tail call
  x + 10       -- the Let body
```

Its MCode (as `UNISON_JIT_DUMP_IR` prints it) is two combinators: `f` itself, whose `Let` holds the call, and the `Let` body as a combinator of its own, taking the whole frame (`n` and `x`) as its two arguments:

```
f:    arity 1
        Let
            Call #g (VArg1 0)    -- the binding: call g with the slot on top
        Lit (MN 10)              -- the body: push 10, add, return
        Prim2 ADDN 1 0
        Yield (VArgV 3)

f.1:  arity 2                    -- the Let body again, as a combinator
        Lit (MN 10)
        Prim2 ADDN 1 0
        Yield (VArgV 3)
```

If `g` returns normally, native `f` just carries on into `x + 10`, and no re-entry point is involved. If `g` exits to the interpreter, we need a re-entry function for the continuation from that point in `f`. Their [properties](#properties-of-re-entry-points) and [how they are generated](#how-re-entry-functions-are-generated) follow.

#### Properties of re-entry points

- **Each one is a `UnisonNativeFn`.** It starts by reading its locals from the Unison stack slots, which is always possible because of [the core rule](#the-core-rule), and begins with the same stack check as any native function.
- **They're optional.** A `Let` with no corresponding re-entry function is resumed in the interpreter, which is always correct. So they can be added gradually.
- **Only the interpreter resumes at one,** after it has done the interpreted work; native code never looks one up. For a call-out, the `Exit` holds the cell of the function that continues after the instruction. For a `Let`, the re-entry point is the `Let` body's own combinator (`f.1` above): the `MCode` emitter already makes every `Let` body a combinator whose arguments are the whole frame, so calling it is resuming at the body, and its [native code cell](#native-code-cells) is carried by the `Let` node and by the `Push` frame.
- **One inside an inlined binding expects the interpreter's picture of the frame.** The generator may compile a `Let`'s binding inline instead of as a call. The interpreter has no such thing, so if native code exits from inside that binding, the frame records describe what the interpreter would have built, a frame for the binding on top of a `Push` for the body, and the code that re-enters there is generated to be entered that way. The mechanism is the [frame base](#the-frame-base).

#### How re-entry functions are generated

The code after a call-out, and the body of a `Let` nested inside an inline binding, are *auxiliary functions* in the same module as their function, named `<name>_r<n>`, each with its own cell; the exit or frame-table entry that leads there holds the cell ([`genAuxFunction`](Codegen.hs#:~:text=genAuxFunction%20%3A%3A)). A `Let` body's combinator is a unit of its group like any other. When each kind gets compiled is in [Re-entry functions on demand](#re-entry-functions-on-demand).

The same continuation is reached from more than one place when a binding is generated both inline and in the auxiliary function for its body, so auxiliary functions are memoized per (section, depth, frame base) ([`AuxMemo`](Codegen.hs#:~:text=type%20AuxMemo)); without this the output grew exponentially with the nesting of bindings. A re-entry function generated later than its module reuses the cells its parent handed out, through the same memo.

### Frames: native code records, Haskell builds

The interpreter adds a `Push` frame to the current continuation before every non-tail call; native code doesn't. So when an exit unwinds through native callers, each writes down a small *frame record*, and the interpreter builds the correct continuation afterwards:

- [What a `K` frame is](#what-a-k-frame-is): the interpreter's continuation, frame by frame.
- [Frame records](#frame-records): the three integers native code writes instead, and the table they index.
- [An example](#an-example): three native calls deep, an exit, and what the interpreter sees.
- [The frame base](#the-frame-base): re-entering inside an inline binding.

#### What a `K` frame is

[`K`](../Stack.hs#:~:text=data%20K) (in `Stack.hs`) is the interpreter's continuation: a linked list, allocated on the Haskell heap, that says what to do after the current code returns. Each constructor in the list is one frame:

- `Push`: "when the current call returns, continue at this section." It records the caller's frame size, pending argument count, a stack guard, the `CombIx` + `RSection` to resume at, and the `Let` body's native code cell. The interpreter pushes one at every non-tail call (`Let`).
- `Mark` / `AMark`: prompts installed by ability handlers (`Reset`). `Capture` copies the list up to the matching prompt.
- `Local`, `Keep`, `CB`, `KE`: saved handler state, values kept alive, callbacks, and the empty continuation.

#### Frame records

Native code can't construct a `Push` frame itself. A `Push` is a Haskell heap object that points to a `CombIx`, an `RSection` and the rest of `K`, and we'd rather native code know nothing about how `K` is represented. So the work is split:

- Native code writes down a *frame record*, three integers, for each frame that needs to exist ([`writeRecord`](Codegen.hs#:~:text=writeRecord%20%3A%3A); on the exit path itself, the [write-back](#write-back) routine appends them). The records go in a per-thread buffer sized from the C stack budget (`max_frames` in [`jit_rt.c`](../../../../cbits/jit_rt.c#:~:text=max_frames)): every native frame that can write records holds C stack, so the budget bounds them. Both writers check the bound before storing and stop the process with a message if it is ever reached (2026-10-24), rather than trusting the sizing.
- After native code returns, the interpreter turns the frame records into real `Push` frames ([`frameRecords`](Native.hs#:~:text=frameRecords%20%3A%3A) reads them from the stack's out-area; [`runNative`](../Machine.hs#:~:text=runNative%20%3A%3A) builds the frames).

A frame record contains:

| Field | Where it comes from |
| --- | --- |
| `Let` body id | a constant in the generated code, identifying which `Let` body to continue with |
| frame size | the frame depth at the call site, which the generator knows statically, so also a constant |
| pending-argument count | `fp - ap` at the call site, a runtime value |

These are the same two sizes the interpreter's `saveFrame` computes. The remaining fields of a `Push` (the `CombIx`, the stack guard and the `RSection`) are fixed for a given `Let`, so they're kept in a Haskell-side *frame table* indexed by `Let` body id ([`Frame` in `Frames.hs`](Frames.hs#:~:text=data%20Frame)), filled in when the function is compiled. This is the same arrangement as the exits table: native code only ever holds an index. A negative index is a `Local` frame instead, carrying a handler environment ([Ability requests](#ability-requests)).

After every non-tail native call, the generated code does the equivalent of:

```c
STATUS r = g(ctx, sp, sp, sp);
if (r != OK) {
  // g is exiting, so we are too. Record the frame the interpreter
  // would have pushed before calling g, then pass g's status along.
  add_frame_record(ctx, LET_BODY_ID, FRAME_SIZE, fp - ap);
  return r;
}
// g returned normally: continue with the Let body
```

- **Order.** Records are written innermost caller first, since that's the order the exit passes through them. `K` has the innermost frame on top, so the interpreter builds from the last record to the first.
- **Inlined `Let`s.** Native code may compile a `Let` binding inline instead of calling it as a function. If it exits partway through that binding, the exiting function itself writes the record for that `Let` before returning (innermost binding first). So a function's own enclosing `Let`s come first, then its callers'. Which `Let`s enclose each exit point is known at compile time.
- **Buffer size.** Each record corresponds to one native call in progress or one inline binding entered, and the C stack guard already limits how deep calls can nest, so the buffer in `Ctx` has a fixed size with room for that many records.
- **Cost.** Nothing on the normal path beyond the status check. On an exit, a few stores per native frame, plus one `Push` allocation per frame in Haskell.

#### An example

```
a n = y = b n
      y + 1
b n = z = c n
      z * 2
c n = k = ask        -- a request whose handler isn't affine: a Capture
      n + k
```

The interpreter, whose continuation is `k0`, calls native `a`. Then `a` calls `b`, `b` calls `c`, and `c` reaches the `Capture`, which it can't run.

| Step | What happens | Frame records in `Ctx` |
| --- | --- | --- |
| 1 | `c` writes its live values to the stack and returns exit index 17 | none |
| 2 | `b` sees a non-`OK` status, records its frame, returns 17 | `b` |
| 3 | `a` sees a non-`OK` status, records its frame, returns 17 | `b`, `a` |
| 4 | The interpreter builds `Push b (Push a k0)` | |
| 5 | The interpreter looks up exit 17 and resumes at the `Capture` with that `K` | |

At step 5 the stack and `K` are exactly what they'd be if the interpreter had run `a`, `b` and `c` itself, and the C stack holds nothing. `Capture` works unchanged. Later, when `yield` pops the frame for `b`, it finds the [re-entry point](#re-entry-points) for that `Let` in `b` and goes back into native code.

#### The frame base

When the generator compiles a `Let`'s binding inline, there is no call and so no frame for the binding while native code runs. But if native code exits from inside that binding, the interpreter must find what it would have built itself: a fresh frame for the binding, starting at the binding's *base* (the depth of the enclosing function's frame at that point), and a `Push` frame for the body. So the frame records written on such an exit describe exactly that, and the code that re-enters inside the binding is generated to match:

- it is entered with the interpreter's frame pointer at the base ([`feBase`](Codegen.hs#:~:text=feBase%20%3A%3A), zero for an ordinary combinator) and recovers the enclosing function's frame pointer by subtracting the base, so that slot offsets stay those of the enclosing function;
- it writes frame records only for bindings nested inside it, since the outer ones are already on `K`;
- the binding's own `Yield` is a return, which lets the interpreter pop the `Push` frame and run the body.

The same mechanism is what gives a `Let` nested inside an inline binding its re-entry point; the body combinator's own code can't serve there, since its arity counts the enclosing function's slots.

### Native code cells

Both the interpreter and native code need to answer the same question before calling a function: has it been compiled, and if so, where is the code? The answer is kept in one place per supercombinator, its *native code cell* ([`NativeCell`](../MCode.hs#:~:text=data%20NativeCell)). What follows: [what a cell holds](#what-a-cell-is), [how both sides reach them](#shared-access-to-cells-from-native-code-and-the-interpreter) and [who reads one](#who-reads-a-cell-and-how-they-find-it).

#### What a cell is

The mutable part of a supercombinator, 32 bytes outside the Haskell heap. `NativeCell` in `MCode.hs` is an empty data type used only as the pointer's tag; the accessors there read and write the words by offset. As a C struct it would be:

```c
struct NativeCell {
  UnisonNativeFn code;   // null until compiled
  int64_t count;         // calls made while code was null, from minus the threshold
  int64_t state;         // 0 not requested, 1 queued, 2 taken by the compile thread
  int64_t verdict;       // the JIT's decision on the combinator
};
```

- `code` is null until the supercombinator is compiled, and after that the address of its `UnisonNativeFn`.
- `count` counts the times the interpreter ran the code itself because `code` was null; it is how a function gets hot. It starts at minus the compilation threshold, so that "hot" is the count reaching zero and the interpreter's test is one comparison with a constant. A count that starts at zero or above never gets there, which is how the JIT is turned off for a cell.
- `state` is how a definition is asked for exactly once: the interpreter moves it from "not requested" to "queued" when it asks for the supercombinator to be compiled ([`claimNativeCell`](../MCode.hs#:~:text=claimNativeCell%20%3A%3A)), and the compile thread moves it to "taken" when it puts it in a batch ([`takeNativeCell`](../MCode.hs#:~:text=takeNativeCell%20%3A%3A)). It moves forward only, by compare-and-swap.
- `verdict` is the compile thread's decision on whether the supercombinator is worth compiling, and whether the interpreter should enter its code ([What isn't compiled](#what-isnt-compiled)). Its low two bits are undecided, worth compiling, not worth compiling or too small to enter; the bits above hold the estimated probability of an exit, which callers' estimates read. [`readNativeEntry`](../MCode.hs#:~:text=readNativeEntry%20%3A%3A) applies the verdict when the interpreter asks for the code pointer: a "too small to enter" cell answers null to the interpreter although the code is there for native callers.
- The code pointer is published with an atomic release store and read with an acquire load ([`writeNativeCode`](../MCode.hs#:~:text=writeNativeCode%20%3A%3A), [`readNativeCode`](../MCode.hs#:~:text=readNativeCode%20%3A%3A); generated code loads a cell with `load atomic ... acquire` too), so that a thread which sees a module's code also sees the pool entries and the exit and frame tables the compile thread wrote before installing it, which on arm64 a plain load would not promise (2026-10-24).

#### Shared access to cells from native code and the interpreter

Cells live outside the Haskell heap, so they never move, the GC ignores them, and both the interpreter and native code can read them.

- Each supercombinator's [`GCombInfo`](../MCode.hs#:~:text=data%20GCombInfo) holds the address of its cell, next to its arity and frame size. `GCombInfo` is unpacked into every partial application closure, so each of those grows by one word.
- A cell's address means something only within one process, so it is not part of code's serialized form: the serializer for compiled program files skips that field, and a combinator that has just been deserialized or emitted (an *unresolved* one) carries a shared placeholder, [`noNativeCell`](../MCode.hs#:~:text=noNativeCell%20%3A%3A). The placeholder never reaches running code: every path that resolves code attaches real cells first, so a deserialized function is compiled like any other.
- Those paths are three: the builtins at startup ([`baseCCache`](../Machine/Types.hs#:~:text=baseCCache%20%3A%3A)), code loaded through the code cache, and a compiled program ([`restoreCache`](../Interface.hs#:~:text=restoreCache%20%3A%3A), for `run.compiled`). Each attaches a cell to every combinator of the group ([`attachNativeCells`](../MCode.hs#:~:text=attachNativeCells%20%3A%3A)) before resolution, and resolution carries them into the resolved form.
- Cells are handed out from a pool ([`NativeCellPool`](../MCode.hs#:~:text=data%20NativeCellPool)) inside the loader's STM transaction, so a retried transaction never leaks memory: when the pool is short, the transaction reports how many cells it needs, the pool is grown outside it, and the transaction is retried. Cells are never freed, since code isn't unloaded.
- A compiled program's file carries no declarations of the types its code uses: the generator takes every field count from the match arms and the objects ([`genDMatchClosure`](Codegen.hs#:~:text=genDMatchClosure%20%3A%3A); the code verifier's module comment in [`MCode/Verify.hs`](../MCode/Verify.hs) says what is and is not checked), and `runStandalone` only starts the JIT as `startRuntime` does for a codebase and registers the file's type numbering, which the one match arm that compares a whole packed tag needs. (Until 2026-10-24 the file carried the types' constructor arities for the generator.)

#### Who reads a cell, and how they find it

| Reader | How it finds the cell |
| --- | --- |
| The interpreter, at a `Call` or `App` ([`enter`](../Machine.hs#:~:text=enter%20%3A%3A), [`apply`](../Machine.hs#:~:text=apply%20%3A%3A)) | from the `GCombInfo` of the supercombinator it's about to run, which it already has in hand |
| Native code calling a known function | the cell's address is a constant in the generated code |
| Native code [calling a function value](#calling-function-values) | from the `GCombInfo` inside the closure |

In each case it's one load and a test for null. If the cell is null, the interpreter interprets the function, and native code exits to the interpreter. Calls within a batch don't use cells: the callee is in the same LLVM module, so the call refers to it directly, and LLVM can inline it. Installing compiled code is one pointer write: readers on other threads see either null or the function pointer, and both are valid, so callers never need recompiling when a callee is compiled later.

For a known function, the compiler has the callee's `GCombInfo` when it compiles a `Call`, reads the cell's address from it and writes that number into the generated code as a constant. This is safe because the cell never moves and compiled code is never saved to disk. If the callee's cell is at address `0x7f3a12004e80`, the generated IR for the call starts like this:

```llvm
%fn = load ptr, ptr inttoptr (i64 139887386840704 to ptr)   ; read the cell
%is_null = icmp eq ptr %fn, null
br i1 %is_null, label %exit_to_interpreter, label %do_call
```

For a function value, native code loads the closure pointer from the boxed stack, clears the tag bits, and loads the cell's address from a fixed offset inside the `GPAp` closure, where `GCombInfo` is unpacked and the address is a raw word.

### The trampoline

Native code never calls into Haskell, and the interpreter never calls native code directly from the middle of its own logic. Every transition in either direction goes through one piece of Haskell code, the *trampoline* ([`runNative`](../Machine.hs#:~:text=runNative%20%3A%3A) in `Machine.hs`, which makes the foreign call itself through [`enterNative`](Native.hs#:~:text=enterNative%20%3A%3A) in `Native.hs`). It's called that because control bounces off it: native code returns to the trampoline, which decides what runs next, which may be more native code. Below: [when the interpreter hands off](#when-the-interpreter-hands-off), [what the trampoline does](#what-the-trampoline-does) with each status, and [its properties](#properties-of-the-trampoline).

#### When the interpreter hands off

The interpreter looks for native code in the places where it would otherwise start interpreting:

- **When calling a known function:** `Call`, and `App` whose target is a known supercombinator ([`enter`](../Machine.hs#:~:text=enter%20%3A%3A)). It checks the supercombinator's [native code cell](#native-code-cells) for a main entry point.
- **When applying a function value** ([`apply`](../Machine.hs#:~:text=apply%20%3A%3A) on a `GPAp`): the same check on the callee's cell, so a compiled function is entered natively however it was reached.
- **When returning into a function:** [`yield`](../Machine.hs#:~:text=yield%20%3A%3A), when it pops a `Push` frame. It checks whether that `Let` has a [re-entry point](#re-entry-points), through the cell the frame carries. This is how execution gets back into native code after an exit.

If it finds native code, the interpreter sets up the call exactly as it would for an interpreted one (arguments moved into place, stack grown if needed) and then passes the function pointer to the trampoline instead of interpreting the body. A call made while the cell is empty adds to the cell's call count, which is what makes the function hot and gets it compiled.

#### What the trampoline does

When calling native code, the trampoline:

1. Fills in `Ctx` (the current addresses of the stacks and the constant pool (the GC may have moved since the last entry), the limits native code checks against, and the handler environment)
2. Calls the native function with an `unsafe` foreign call ([`unison_jit_enter`](../../../../cbits/jit_rt.c#:~:text=int64_t%20unison_jit_enter%28UnisonNativeFn), which sets up the per-thread context, calls the function and marks the [write barrier](#the-write-barrier) on the way out)
3. Read the stack pointers and the frame records back from the out-area past the unboxed stack's last slot (the C entry point copies them there, with up to eight records inline and more in a `malloc`ed copy, so a round trip allocates nothing on the Haskell heap)
4. Build `K` frames from the records on top of the `K` it was entered with
5. Then act on the status:

| `STATUS` | What the trampoline does next |
| --- | --- |
| `OK` | The function finished and its result is on the stack. Hand the result to the continuation with `yield`, as when an interpreted function returns. |
| exit index of a `Resume` | Continue interpreting at the recorded section ([`eval`](../Machine.hs#:~:text=eval%20%3A%3A)). |
| exit index of a `Reenter` | Yield to the scheduler, so the runtime can switch threads or run a GC. Then enter the function the exit names again. |
| exit index of a `GrowStack` | Grow the Unison stack ([`ensureGenerously`](../Stack.hs#:~:text=ensureGenerously%20%3A%3A), by at least its current size), then enter the function the exit names again. |
| exit index of a `CallOut` | Run the one recorded instruction with the interpreter's [`exec`](../Machine.hs#:~:text=exec%20%3A%3A). Then enter the re-entry function, so the rest of the function runs natively. If the instruction raised an exception, don't re-enter: route it to the current exception handler, as the interpreter does for that instruction (the handler's continuation is the re-entry function, so the rest of the function is still native afterwards). |
| `EXIT_ERROR` | Fail with an internal error. |

Two details:

- A `Reenter` or `GrowStack` exit names the function to call again, which after tail calls need not be the one entered here. Its cell can still be empty: the functions of a module call each other directly, and one can be running before the compile thread has written the cell of another that it called. The cell is about to be filled, so the trampoline waits for it ([`again`](../Machine.hs#:~:text=again%20cell%20henv%20stk%20k%20%3D%20wait), polling with a short delay, up to a limit).
- A `CallOut` re-enters only if the cell has code *and* the instruction pushed exactly the recorded number of values with the frame pointer unmoved; otherwise it interprets the section after the instruction, and if the cell was empty, counts the miss against the cell (that count is what asks for a deferred re-entry function). MCode's `Ins` doesn't say how many values an instruction pushes, so [`pushCount`](Codegen.hs#:~:text=pushCount%20%3A%3A) lists every constructor: a new instruction fails to compile until it is classified, and a wrong entry costs a resume, never correctness.

#### Properties of the trampoline

- **Neither stack grows across transitions.** By the time native code returns to the trampoline, all its C stack frames are gone. On the Haskell side, the trampoline's moves (`yield`, continue interpreting, re-enter native code) are all tail calls. So a program can go back and forth between native and interpreted code indefinitely.
- **The interpreter and native code interleave freely.** An interpreted function can call a native one, which can exit to the interpreter for an unknown call, whose callee may itself be native. Each side only ever sees the shared state: the Unison stack and `K`.
- **Nothing else in the interpreter changes.** Besides the checks above, the interpreter runs as it does today. When it resumes after an exit it can't tell that native code was involved, because of [the core rule](#the-core-rule).
- **Cost of a transition.** Entering native code costs an `unsafe` foreign call plus a few stores into `Ctx`, and a round trip allocates nothing; it is about 37 ns, and everything on this path showed in the profile when it was more.

### What gets compiled, and when

The compilation unit is a batch of supercombinators, compiled as a single LLVM module. It is highly beneficial to compile multiple related supercombinators together, so that inlining and further optimizations can be applied that cross function boundaries. The driver is [`compileThread`](../JIT.hs#:~:text=compileThread%20%3A%3A) in `JIT.hs`, with the per-module work in [`Compile.hs`](Compile.hs). The parts:

- [Hot functions and batches](#hot-functions-and-batches): the threshold, the queue, and how a batch grows along the call graph.
- [Units and the two passes](#units-and-the-two-passes): what a module is made of and how it is generated.
- [Private copies](#private-copies): compiling a small callee again so that a new module can inline it.
- [Re-entry functions on demand](#re-entry-functions-on-demand): the continuations after call-outs, generated when they are used.
- [What isn't compiled](#what-isnt-compiled): the static estimate that leaves exit-heavy functions interpreted.
- [Installation, startup and shutdown](#installation-startup-and-shutdown): how code goes live, and how LLVM is loaded.

The thresholds and weights below are environment variables, parsed and documented in [`Config.hs`](Config.hs#:~:text=data%20Config); the diagnostics (logging, IR dumps, statistics, traces, stress modes) are in [development.md](development.md#looking-inside).

#### Hot functions and batches

* Each supercombinator yet to be native-compiled (an "interpreted" function) keeps a count of how many times it has been called, in its cell.
* When this count reaches a threshold (N, default 100, [`UNISON_JIT_THRESHOLD`](Config.hs#:~:text=thresh%20%3C%2D)), it is said to be "hot" and the definition is queued (idempotently, [`jitRequestGroup`](../JIT.hs#:~:text=jitRequestGroup%20%3A%3A)) for a background compile thread. Any combinator of a group reaching N gets the whole group compiled (local functions have their own cells, so a hot local loop inside a function called once counts). The queue is bounded; a request it refuses clears the request state again and sets the count to ask after another 1024 calls. The compile thread is placed on a different capability than the interpreter's, so that neither slows the other down.
* We use the call graph of the code to pull related definitions into the same batch ([`formBatch`](../JIT.hs#:~:text=formBatch%20cache%20callers%20grp%20%3D)). The compile thread grows the batch from the hot definition one neighbour at a time, using [Prim's algorithm](https://en.wikipedia.org/wiki/Prim%27s_algorithm) over the call graph in both directions (callers and callees), taking next the candidate with the most estimated calls into the batch, as long as that estimate, divided by the candidate's size relative to a typical definition ([`groupSize`](../JIT.hs#:~:text=groupSize%20%3A%3A), in units of `UNISON_JIT_SIZE_UNIT`, 500), reaches a gate (N/4, `UNISON_JIT_BATCH_GATE`), up to a batch of size B (32, `UNISON_JIT_BATCH`). (The division keeps a large definition that runs once, such as a program's main body with many call sites into hot code, from joining the batch of everything it calls: compiling it would cost most of the module's time and save a handful of calls.) A candidate under the gate stays a candidate, since a later member may add edges to it.
* The estimate for an edge ([`edgeCalls`](../JIT.hs#:~:text=edgeCalls%20%3A%3A)) is the call count of the combinator holding the call site (every site is taken to run once per call of its combinator; a site in a local loop counts the loop's own calls), so a callee can join on the strength of its caller's count before it has been called at all, and a caller joins by its own count. The dependents direction tends to matter more since a callee is called at least as often as its caller, so it usually gets hot first. A neighbour whose own request is already waiting in the queue skips the gate. The code cache records only what each definition calls, so the compile thread keeps the reverse index ([`indexCallers`](../JIT.hs#:~:text=indexCallers%20%3A%3A)) over every group in the cache; what it can't know are callers loaded later and calls through closures.
* Once compiled, pointers to native code for each supercombinator are installed while the program runs, one pointer write per supercombinator. Nothing assumes a batch appears all at once.

#### Units and the two passes

Compilation ([`Compile.hs`](Compile.hs)) works on *units*, one LLVM function each: a combinator (an entry point or local function of a group), or a re-entry function ([`groupUnits`](Compile.hs#:~:text=groupUnits%20%3A%3A)). A group (a top-level definition with its local functions and `Let` bodies) is split into the units compiled now and those that wait to be asked for. Function names are `u<group>_<index>`, with a suffix when a second runtime in the process reuses group numbers.

A module is generated twice ([`compileUnits`](Compile.hs#:~:text=compileUnits%20%3A%3A)): the first pass finds out which functions compile and how many exits, frames and cells each needs (its cells are a counting supply); the exits, frames and cells are then registered, and the second pass generates with the real bases, which also knows which functions of the module compiled and so can call them directly and as workers. A function that fails as a worker in the first pass is generated in the uniform form in the second. The indices registered after the first pass are kept by the second, which assumes a call site has the same exits either way.

#### Private copies

A small function that is already compiled and installed can be compiled again and included as a private copy in a later batch that calls it ([`copyUnits`](Compile.hs#:~:text=copyUnits%20%3A%3A), named `_c<n>`, with internal linkage), so that the call is direct and LLVM can inline it; the original keeps serving every other caller and the copy is not installed in its cell. This adds some code duplication but allows for more inlining. The criterion is the callee's estimated work per call (its saving, below, at most `UNISON_JIT_COPY`, 40) and that it doesn't loop or recurse, not its size. In testing we saw such copies were almost always inlined and deleted by the LLVM optimizer.

#### Re-entry functions on demand

[Re-entry points](#re-entry-points) are compiled at three different times, by how sure it is that they will be used:

- **With their module**, when the call-out is certain: the continuation after an instruction with no native version.
- **When asked for**, when the call-out is a slow path that may never be taken: the continuation after the slow path of an instruction with a native fast path gets its name and cell with the module, but its code is *deferred* ([`Deferred`](Codegen.hs#:~:text=data%20Deferred), [`deferredUnit`](Compile.hs#:~:text=deferredUnit%20%3A%3A)) until the interpreter has found the cell empty often enough. Such requests are held and compiled together when there are B of them or none has arrived for `UNISON_JIT_REENTRY_WAIT` ms (20); a definition's request is served ahead of them.
- **As units of their group**, for `Let` bodies: compiled with the group, or later when asked for, by the misses counted when the body's `Push` frame is popped with an empty cell ([`leap`](../Machine.hs#:~:text=leap%20%28Push)).

An exit the code causes (a callee that isn't compiled, a handler, a call-out) recurs at the same `Let` on every call, so a body asked for that way is worth having, and there are only as many as the code has `Let`s. An *ad hoc* exit, the poll or stack growth ([`isAdHoc`](Exits.hs#:~:text=isAdHoc%20%3A%3A)), lands wherever the program happens to be: over a long run it interrupts a hot function at every `Let` it has, and each body is the function's whole suffix from there, so counting those misses would in time generate about n²/2 of an n-`Let` function again. So the trampoline tells the two apart as it builds frames from the records ([`pushRecord`](../Machine.hs#:~:text=pushRecord%20bstk)): a frame for an ad hoc exit through a `Let` whose body has no code carries no cell, its pop interprets the body without counting, and [`noteAdHocExit`](../JIT.hs#:~:text=noteAdHocExit%20%3A%3A) lets at most `UNISON_JIT_REENTRY_ADHOC` (3) bodies per group be asked for this way; since a tick lands where the time goes, those are the `Let`s most worth resuming natively. `UNISON_JIT_STATS` reports both counts.

#### What isn't compiled

Native code saves the interpreter's overhead, a few nanoseconds per instruction; an exit and the way back cost several times that. So before a function is compiled, its MCode is walked ([`Estimate.hs`](Estimate.hs), [`judge`](Estimate.hs#:~:text=judge%20%3A%3A)) to estimate, for an average path from entry to return, the overhead native code would save and the number of exits it is *certain* to take (instructions with no native version, calls to handlers, and calls to functions that are themselves not worth compiling). If the exits, weighted by the cost of a round trip, exceed the saving, the function is left to the interpreter and its cell records this verdict. Callees that haven't been judged are judged on the way, so the answer doesn't depend on the order in which functions get hot, and "not worth compiling" spreads to callers that do little but call such functions. Exits that only happen on a slow path (the poll, stack growth, a fast path's miss) don't count. What the rule can't see is the behaviour of function values: a loop that calls a closure which turns out not to be native takes an exit per iteration.

The weights are in [`Estimate.hs`](Estimate.hs); the two settings:

- `UNISON_JIT_EXIT_COST` (E, 7): a function is left interpreted if `exits × E > saving`, the saving being one unit per instruction or branch and three per call or `let`.
- `UNISON_JIT_ENTRY_COST` (3 units, about 20 ns): a function whose average path saves less than this, and that has no loop, gets a third verdict, *too small to enter*. It is compiled, and native callers call it directly, but the interpreter keeps interpreting it, since entering native code would cost more than it saves.

#### Installation, startup and shutdown

Each function's symbol is looked up and its cell written, one at a time. The re-entry functions left for later become pending before any code that can exit to them is installed.

LLVM isn't linked statically: [`jit_llvm.c`](../../../../cbits/jit_llvm.c) finds and loads it with `dlopen` (where it looks: [development](development.md#building)), resolves every function it uses with `dlsym`, and refuses any LLVM version older than 20. 

Some notes on how we use LLVM:

* A module is optimized with the pipeline in [`defaultPasses`](Config.hs#:~:text=defaultPasses%20%3A%3A) (`UNISON_JIT_PASSES`): `default<O2>`'s shape with the passes our code can't use left out, which produces the same code on the suite in 60% of the time ([2026-10-09 pipeline](benchmarks/2026-10-09-pipeline.md)).
* Right after loading the JIT switches LLVM's two machine instruction schedulers off: they were 73% of code generation time on the suite's largest module, since our functions are long straight-line blocks of slot traffic and the schedulers' dependency graphs grow superlinearly with block length, and an out-of-order core reorders at run time anyway. ucm does this at startup when the JIT is on, together with the startup checks (layouts, helpers), and prints whether the JIT is activated; if LLVM isn't found or any check fails, the JIT is off, the reason is printed, and the program runs interpreted.
* The load happens once ([`initJIT`](../JIT.hs#:~:text=initJIT%20%3A%3A) remembers its outcome), so a runtime started by another host gets the same answer.
* Everything the JIT keeps lives in one type ([`JIT`](../JIT.hs#:~:text=data%20JIT), one per process): the settings, read from the environment by [`readConfig`](Config.hs#:~:text=readConfig%20%3A%3A) when ucm starts, the exits and frames tables, the constant pool, the compile thread's queue and the compiler's state. 
* ucm makes it ([`newJIT`](../JIT.hs#:~:text=newJIT%20%3A%3A)) before it starts a runtime and gives it to every runtime it starts, since the compiled code is shared between them: generated code refers to the tables by index and native code in one module calls into another directly, so there is no way to give each runtime its own. The code cache carries the handle ([`CCache`](../Machine/Types.hs#:~:text=jit%20%3A%3A)'s `jit`), which is how the interpreter reaches it on every path; nothing in the JIT is a global.
* After startup LLVM is used from the compile thread only (and from the loading thread in eager mode, which has no compile thread).
* For graceful shutdown, an `atexit` handler in the LLVM glue waits for a compile in flight and parks the compile thread, since otherwise `hs_exit` doesn't wait for a thread in a safe foreign call and this could lead to crashes with LLVM's teardown logic running alongside a batch compilation.

### Code generation

The generator is [`Codegen.hs`](Codegen.hs), one combinator ([`genFunction`](Codegen.hs#:~:text=genFunction%20%3A%3A)) to LLVM IR as text, handed to LLVM as one buffer per module (`UNISON_JIT_DUMP_IR=dir` writes each module's IR before and after optimization). The parts:

- [Slots](#slots): the Unison stack as registers.
- [Workers, wrappers and the fast entry](#workers-wrappers-and-the-fast-entry): arguments and results in registers between functions of one module.
- [A worked example](#a-worked-example): the optimized output for a small recursive function.

The generator is strict throughout: its text is `Unison.Util.Text`, its sequences are `Unison.Util.Deque`, its pairs and record fields are strict ([`Strict.hs`](Strict.hs)), and the state is evaluated at every step, so no thunk over a module's text outlives the step that made it.

#### Slots

The generator doesn't allocate registers itself. Within a function, every Unison stack slot is an LLVM `alloca`, `%u<k>` for the unboxed word and `%b<k>` for the boxed pointer of the slot at frame offset `k`, and the generated code loads and stores those as the MCode reads and writes slots. LLVM's `mem2reg` pass then turns the allocas into SSA registers, which is how clang handles C locals too. This works because the frame depth at every point is known statically, so MCode's "slot `i` from the top" is always a fixed offset. The real Unison stack is touched only at entry, before a tail call through the stack, and on exit paths. A boolean produced by a comparison stays an `i1` register until something stores it where it escapes; a `DMatch` on it is a branch.

#### Workers, wrappers and the fast entry

The [uniform signature](#the-native-signature) passes everything through the Unison stack: the caller stores the arguments, the callee loads them, and the result comes back the same way. That is right for the trampoline and for calls through a cell, but between functions compiled together it is all overhead, and LLVM can't remove it, even when it inlines, because the stack is reachable from `Ctx`.

So a function is also compiled as a **worker**: an internal LLVM function `(ctx, fp, u1, b1, ..., un, bn) -> {status, u, b}` with LLVM's `tailcc` calling convention, under which a call marked `musttail` is guaranteed to be a tail call even between functions of different signatures (the C convention allows `musttail` only between identical ones). The arguments are parameters, the result is two return values, and the Unison stack isn't written at all unless something exits. The function its cell points to becomes a **wrapper** with the uniform signature: load the arguments from the stack, call the worker, store the result. Calls between functions of one module go to the worker directly, and an inlined worker is plain LLVM IR code in its caller. The frame keeps its place on the Unison stack: a worker is passed the frame pointer its frame would have, and its caller's stack check leaves room for its arguments there; if it exits it writes its slots to that place, and each native caller above it writes its own frame and record as the status passes up, so the interpreter sees the same stack either way. The worker doesn't take `ap`: the wrapper hands a call with pending arguments (`ap` differs from `fp`) to the interpreter before the worker runs.

Not every supercombinator can be a worker. A worker returns exactly one value, as every Unison function does, but the unit of compilation is an MCode supercombinator, and some of those return several: a `Let` binding that binds several variables (`(a, b) = ...` style destructuring, or a primitive such as `LOAD` that pushes two results) is a combinator returning all of them to its body. Such a combinator is compiled in the uniform form only. [`workerShape`](Codegen.hs#:~:text=workerShape%20%3A%3A) looks at a combinator's `Yield`s before generation starts and rules out the ones that visibly return several values; if generation of a worker then meets a `Yield` with the wrong number of values anyway, it gives the worker up and the combinator gets the uniform form in the second pass.

- *Tail calls.* Worker to worker is a real tail call (`musttail`, which `tailcc` allows between different signatures; on Windows, whose x86-64 backend refuses guaranteed tail calls, workers are `fastcc` and the call a plain `tail call`, a sibling call when the arguments fit and an ordinary call otherwise, bounded by the C stack guard). A worker can't tail call a function with the uniform signature, since the return types differ: it puts the arguments on the stack and returns status -2 with the code pointer and the stack pointer in the other two return values; the wrapper at the bottom makes the call as a real tail call, and a worker that had called this one with a plain call makes it as a plain call. Either way the C stack doesn't grow.
- *Checks.* A worker checks the Unison stack, the poll and the C stack once, at entry, and looks at the allocation budget only if it allocates.
- *The fast entry.* Many functions start with a test and return at once in the base case. For such a worker a second, tiny function is what callers call ([`genFastEntry`](Codegen.hs#:~:text=genFastEntry%20%3A%3A)): it holds only the paths that can't exit, call or loop, which need nothing checked, and returns from them directly; any other path tail calls the full worker, which starts again from the top (what it repeats is a few instructions without side effects). The fast entry takes the worker's name, `<name>_w`, and the full worker becomes `<name>_wf`. The base case then costs no stack check, no poll, and none of the register saving the full worker does on behalf of its exit paths. Worth 15% on `fib`.
- *Cold paths.* Every branch to an exit or a slow path carries a branch weight (`!prof`). Without them LLVM kept the values the exit paths need in callee-saved registers, which every call then paid to save and restore; this alone took `fib 20` from 89 to 74 µs before workers existed.

#### A worked example

`sumTo n = if Nat.eq n 0 then 0 else n + sumTo (Nat.drop n 1)` is one combinator: a test, a `Let` whose binding is the recursive call, and the addition. Below is LLVM's output after its O2 pipeline (a dump of 2026-10-20), shortened: the three blocks that write the frame back to the Unison stack and return an exit status (the stack-growth check, the poll, and the unwind when the recursive call comes back with a status other than `OK`) are reduced to a comment each, since they are alike: two stores of the live slot into the argument block `%xargs` and a call of the write-back routine with the site's descriptor, a constant of the module ([Write-back](#write-back)). The generated IR before optimization is about four times longer.

The worker. Its arguments are the argument's unboxed word `%a.u1` and boxed pointer `%a.b1` (for a `Nat`, the word is the number and the pointer is the type-tag closure for `Nat`, which the result carries too), and it returns `{status, u, b}`. What callers call is the [fast entry](#workers-wrappers-and-the-fast-entry), `@u652_0_w`: the test and the base case, which needs no check of any kind, and for the other arm a tail call of the full worker `@u652_0_wf`, which LLVM has inlined here (the `.i` suffixes), checks included. The recursive call is a direct call from worker to worker:

```llvm
@u652_0_wf.x0 = private unnamed_addr constant [7 x i64] [i64 3, i64 0, i64 0, i64 1, i64 1, i64 0, i64 1]   ; the descriptor of the entry exits: slot 1 live, no frame record
@u652_0_wf.x3 = private unnamed_addr constant [10 x i64] [...]                                              ; the unwind's: slot 1 live, one record for the Let

define internal tailcc { i64, i64, ptr } @u652_0_w(ptr %ctx, i64 %fp.in, i64 %a.u1, ptr %a.b1) {
head:
  %xargs.i = alloca [4 x i64]                 ; the argument block of the write-back calls
  %c.2 = icmp eq i64 %a.u1, 0
  br i1 %c.2, label %yield.11, label %false.16

yield.11:                                     ; the base case: n == 0
  %pool = load ptr, ptr (ctx + 16)            ; the constant pool
  %tag.nat = load ptr, ptr (pool + 24)        ; the Nat type-tag closure
  %ret.15 = insertvalue { i64, i64, ptr } { i64 0, i64 0, ptr undef }, ptr %tag.nat, 2
  ret { i64, i64, ptr } %ret.15               ; status OK, value 0

false.16:                                     ; the full worker, inlined: room on the Unison stack?
  %pool.i = load ptr, ptr (ctx + 16)
  %stack.size.i = load i64, ptr (ctx + 24)
  %tag.nat.i = load ptr, ptr (pool.i + 24)
  %need.i = add i64 %fp.in, 11
  %room.i = icmp slt i64 %need.i, %stack.size.i
  br i1 %room.i, label %head.i, label %grow.i, !prof !0

head.i:                                       ; the poll (another thread sets HpLim to null)
  %hplim.p.i = load ptr, ptr (ctx + 32)
  %hplim.10.i = load volatile ptr, ptr %hplim.p.i
  %stop.11.i = icmp eq ptr %hplim.10.i, null
  ...                                         ; and the C stack depth check, or-ed in
  br i1 %stop.17.i, label %exit.18.i, label %body.24.i, !prof !2

body.24.i:
  %r.45.i = add i64 %a.u1, -1                 ; Nat.drop n 1
  %fpk.59.i = add i64 %fp.in, 5               ; the callee's frame pointer
  %r.60.i = call tailcc { i64, i64, ptr } @u652_0_w(ptr %ctx, i64 %fpk.59.i, i64 %r.45.i, ptr %tag.nat.i)
  %st.61.i = extractvalue { i64, i64, ptr } %r.60.i, 0
  %cond = icmp eq i64 %st.61.i, 0
  br i1 %cond, label %cont.65.i, label %unwind.69.i, !prof !3

cont.65.i:                                    ; n + the result
  %r.u.62.i = extractvalue { i64, i64, ptr } %r.60.i, 1
  %r.93.i = add i64 %a.u1, %r.u.62.i
  %ret.104.i = insertvalue { i64, i64, ptr } { i64 0, i64 undef, ptr undef }, i64 %r.93.i, 1
  %ret.105.i = insertvalue { i64, i64, ptr } %ret.104.i, ptr %tag.nat.i, 2
  ret { i64, i64, ptr } %ret.105.i

grow.i:        ; store n into %xargs, call @unison_jit_exit_frame(ctx, @u652_0_wf.x0, fp, ap, xargs), return status 3256 (grow the stack)
exit.18.i:     ; likewise, status 3258 (re-enter this function when the thread next runs)
unwind.69.i:   ; likewise with @u652_0_wf.x3, which adds the frame record for the Let; pass the callee's status on
}
```

The wrapper has the uniform signature. It loads the argument from the Unison stack, calls the worker, and on `OK` stores the result where a `Yield` would put it and sets the stack pointers in `Ctx`; any other status is an exit the worker has already prepared, and is passed on:

```llvm
define i64 @u652_0(ptr %ctx, i64 %ap, i64 %fp.in, i64 %sp) {
  %pending.not = icmp eq i64 %ap, %fp.in
  br i1 %pending.not, label %enter, label %pending.exit, !prof !0

enter:
  %ustk = load ptr, ptr %ctx                  ; the unboxed and boxed stacks
  %bstk = load ptr, ptr (ctx + 8)
  %ix1 = add i64 %ap, 1
  %u1 = load i64, ptr (ustk + ix1)            ; the argument
  %b1 = load ptr, ptr (bstk + ix1)
  %r = call tailcc { i64, i64, ptr } @u652_0_w(ptr %ctx, i64 %ap, i64 %u1, ptr %b1)
  %st = extractvalue { i64, i64, ptr } %r, 0
  %cond = icmp eq i64 %st, 0
  br i1 %cond, label %ok, label %exit

ok:                                           ; the result goes where Yield would put it
  store i64 %r.u, ptr (ustk + ix1)
  store ptr %r.b, ptr (bstk + ix1)
  store i64 %ap, ptr (ctx + 40)               ; ap, fp, sp
  store i64 %ap, ptr (ctx + 48)
  store i64 %ix1, ptr (ctx + 56)
  ...                                         ; raise the high-water mark of written slots
  ret i64 0

exit:
  ret i64 %st

pending.exit:                                 ; the stack pointers go back as they came
  ...
  ret i64 3263                                ; status: resume this function in the interpreter
}
```

(The `ptr (ctx + 16)` forms are a shorthand for the `getelementptr` and `load` pairs in the real output. The module also holds a third function, `@u652_2`: the re-entry point for the `Let`'s body, which the interpreter calls when the recursive call exited and it has run the rest; it has the uniform signature, loads the frame's six slots from the Unison stack, and adds the two values. The dump is reproduced with a transcript that defines `sumTo` and evaluates it, under `UNISON_JIT=eager UNISON_JIT_DUMP_IR=DIR`.)

### Calls

A call from native code is one of three things: a direct call to a function in the same module, a call through a [cell](#native-code-cells) to a known function compiled elsewhere, or a call to a function value found on the stack ([`genApp`](Codegen.hs#:~:text=genApp%20%3A%3A)). The parts of the design:

- [Calling function values](#calling-function-values): what a `GPAp` holds, and how native code calls through it.
- [Tail calls](#tail-calls): loops without C stack.

#### Calling function values

Higher-order code is everywhere in Unison, so native code has to be able to call a function it was passed. In `List.map f xs`, the call to `f` is an `App` whose target is a value on the stack, not a supercombinator known at compile time. If each such call meant an exit to the interpreter, `List.map` would exit once per element.

**What a function value is.** For an ordinary function or lambda, it's a [`GPAp`](../Stack.hs#:~:text=%7C%20GPAp) closure, which holds:

| Field | Meaning |
| --- | --- |
| [`CombIx`](../MCode.hs#:~:text=data%20CombIx) | which supercombinator it refers to |
| [`GCombInfo`](../MCode.hs#:~:text=data%20GCombInfo) | that supercombinator's arity, frame size, code, and the address of its [native code cell](#native-code-cells) |
| captured arguments | values already supplied, such as a lambda's free variables |

**What native code does.**

1. Check that the value is a `GPAp`. This is a test on the pointer's tag bits and the info pointer.
2. Read the arity and the number of captured arguments.
3. If captured plus supplied equals the arity, the call is exactly saturated. Copy the supplied arguments onto the Unison stack, then the captured ones above them, which is the layout the interpreter's [`apply`](../Machine.hs#:~:text=apply%20%3A%3A) produces.
4. Read the address of the native code cell from the closure's `GCombInfo`, and load the function pointer from the cell.
5. Call it like any other native function: a tail call if the `App` is in tail position, and otherwise a call followed by the status check.

Compared to a call to a known function, this adds a few loads and compares.

**Building function values.** A known combinator used as a value (`App (Env f)` with no arguments, which is how a lambda becomes a closure) is a `GPAp` with nothing captured: a constant, kept in the [pool](#the-constant-pool) and built once. A `Name` instruction, which adds arguments to a function value, has two cases. When the function is a known combinator (a local function closing over variables, or a known function given too few arguments, the common case) the closure is built in place ([`genNameInline`](Codegen.hs#:~:text=genNameInline%20%3A%3A)): the pool's constant for the combinator is the template, copied into one heap chunk with two fresh segment arrays holding the arguments, as a wide constructor is built. When the function is a value on the stack, a C helper ([`unison_jit_name`](../../../../cbits/jit_rt.c#:~:text=unison_jit_name)) copies the closure with the new arguments prepended to its arrays; the arguments go to it on the Unison stack, in the scratch slots above the frame's depth, so neither case limits how many there are (the helper took at most four as C arguments until 2026-10-20, and a lambda over more variables was a call-out).

**What still exits to the interpreter.**

| Case | Why |
| --- | --- |
| The callee isn't compiled yet | there's nothing to call; it will be compiled once it's hot |
| Too many arguments | the extra ones stay pending and are applied to the result, which needs more machinery |
| Too few arguments, to a function value | the result would be a new `GPAp` with the arguments captured; for a *known* function given too few arguments the generator builds that in place ([Building function values](#calling-function-values)), but for a value on the stack it is the interpreter's |
| The value isn't a `GPAp`, for example a captured continuation | rare |
| The captured arguments don't fit in the space left on the Unison stack | they are copied before the callee's own entry check runs |
| A tail call made while the caller has pending arguments | the interpreter applies them to the result |

These exits work as described in the [exits table](#when-native-code-exits): in a `Let` binding, write a frame record and `Resume` at the binding; in tail position, `Resume` at the `App`.

#### Tail calls

Loops in Unison are tail calls, so native code must run them without using any C stack.

- **A self tail call** is compiled as a branch back to the top of the function. The loop state is in the argument slots (or registers), so this is an ordinary native loop.
- **A tail call to another function** uses LLVM's `musttail`, which guarantees the call reuses the caller's C stack frame. Under the C calling convention `musttail` requires the caller and callee to have the same signature, which the uniform `UnisonNativeFn` type provides. Workers have differing signatures, so they use LLVM's `tailcc` convention, which guarantees tail calls without that restriction.
- **A tail call returns the callee's `STATUS` directly** to the caller's caller, so there's no status check and no `K` frame to record.

Only non-tail calls use C stack, and those are covered by the [stack guards](#stack-growth-and-preemption).

### Stack growth and preemption

Native code can't grow a stack or give up the CPU by itself. Both are handled the same way: native code checks a condition at known points, and if the check fires, it exits to the interpreter, which does the work in Haskell and then calls back into native code. The three checks ([`entryBlock`](Codegen.hs#:~:text=entryBlock%20%3A%3A), [`genHead`](Codegen.hs#:~:text=genHead%20%3A%3A), [`cStackGuard`](Codegen.hs#:~:text=cStackGuard%20%3A%3A)):

- [The C stack](#the-c-stack): a depth limit, beyond which calls go through the interpreter.
- [The Unison stack](#the-unison-stack): one check per function for its whole frame.
- [Preemption](#preemption): the poll that lets the Haskell runtime switch threads and collect garbage.

Stress modes (`UNISON_JIT_STRESS=poll=N,callee=N,cstack=N,alloc=N,ustack=N,...`) fire the poll every N entries, treat every N-th callee as not compiled, lower the C stack limit, shrink the budget and start with a tiny Unison stack, so that the exit paths run in the tests ([development](development.md#testing)).

#### The C stack

Unison programs can recurse deeply. Above a depth limit, native code exits instead of calling, and the rest runs from the interpreter with heap-allocated frames. The limit is the C stack pointer compared with a limit in `Ctx`. The limit is set at every entry from the trampoline: a budget ([`cstack_budget`](../../../../cbits/jit_rt.c#:~:text=static%20int64_t%20cstack_budget), 1 MiB) below the trampoline's own position, bounded below by the thread's stack floor plus a reserve, so the floor only binds on small thread stacks (GHC's worker threads on macOS get 512 KB). A function with the uniform signature checks it before each non-tail call; a worker checks it once at entry, if it makes calls. Beyond the limit, recursion alternates between native and interpreted frames, one interpreted `Let` per limit hit.

#### The Unison stack

Every supercombinator has a maximum frame size that's known at compile time: the most Unison stack it can use, including the argument slots for any call it makes. Every native function starts by checking that a frame of that size (plus one slot, as the interpreter's [`ensure`](../Stack.hs#:~:text=ensure%20%3A%3A) demands) fits in the space remaining. If it fails, the function returns a `GrowStack` exit before doing anything else; a wrapper's arguments are already in their stack slots, and a worker writes its parameters there. Once the entry check has passed, every slot the function could write, including those written on its exit paths, is inside the space that was checked. Checking at entry rather than at the call site is one check per function, not one per call, and works for calls to function values, where the caller doesn't know the callee's frame size. Only the interpreter reallocates the stack arrays, so their addresses stay valid for as long as native code runs. The [fast entry](#workers-wrappers-and-the-fast-entry) has no paths that can exit, so it makes none of these checks.

#### Preemption

Haskell threads aren't preempted at arbitrary points: the runtime's timer sets a flag on the capability, and compiled Haskell code notices the flag the next time it allocates, then hands control to the scheduler. Native code inside an `unsafe` foreign call never reaches that check. Left alone, a long-running native loop would starve the other Haskell threads on its capability, stall the whole program whenever any thread needs a GC (a collection waits for every capability to stop), and delay `killThread` and timeouts. So native code polls at the entry of every native function, which includes the top of a loop, since a self tail call branches back there: Unison has no loops other than calls, so any code that runs for an unbounded time must keep entering functions, and everything between two function entries is straight-line code of bounded length. The poll checks the capability's context-switch request (a volatile load of `HpLim` through the pointer in `Ctx`; without `volatile`, LLVM hoists the load out of the loop and the poll never fires) and, in code that allocates, the [allocation budget](#the-allocation-budget); when it fires the function returns a `Reenter` exit, and when the thread next runs the interpreter enters the same function again. It costs a load, a compare and a branch that's almost never taken, per function entry.

### Memory

Native code allocates real Haskell heap objects, in the same allocation block compiled Haskell uses. This is safe because GHC can't run a GC on this capability during an `unsafe` foreign call, and native code keeps no heap pointers once it returns. Heap constants (`Reference`s, type tags, `GCombInfo`, literals) come from the [constant pool](#the-constant-pool), never from addresses baked into code. The parts:

- [Allocation](#allocation): the bump pointer and the budget.
- [Layouts](#layouts): the constructor shapes native code reads and writes, probed at startup.
- [Pointer tagging](#pointer-tagging): reading and writing tagged pointers.
- [The write barrier](#the-write-barrier): telling a generational GC about stores into old objects.
- [The allocation budget](#the-allocation-budget): making sure a GC still happens.

#### Allocation

For background: compiled Haskell code allocates by bumping a pointer. The runtime gives each capability a *nursery*, a chain of blocks of fresh memory, and compiled code keeps two pointers to the block it is filling in machine registers throughout execution: `Hp`, the next free word, and `HpLim`, the end of the block. Allocating an object is adding its size to `Hp`, comparing against `HpLim`, and writing the object's words; only when the object doesn't fit does the code call into the runtime for the next block, and when the nursery is used up that call runs a minor GC. So an allocation that fits costs an add, a compare and the stores, with no call and no locking.

Unison JIT-compiled code allocates the same way. `Ctx` carries a copy of the capability's current allocation block's free pointer and end (`hp`, `hp_lim`), set by the trampoline at entry from the register table's `rCurrentAlloc`, the block the RTS's `allocate` uses for allocation from C. Generated code ([`allocWords`](Codegen.hs#:~:text=allocWords%20%3A%3A)) and the C helpers bump `hp` against `hp_lim` and call [`unison_jit_alloc_words`](../../../../cbits/jit_rt.c#:~:text=void%20%2Aunison_jit_alloc_words%28UnisonJitCtx%20%2Actx%2C%20int64_t%20n%29%20%7B) only when the object doesn't fit or there is no block yet. 

The slow path writes `hp` back to the block, calls `allocate` (which may move on to a new block) and reloads the copy; the trampoline writes it back on return and charges the thread's allocation counter for the words taken inline.

Notes:

* GHC's own `Hp` can't be used: on arm64 and x86-64 it is a callee-saved machine register, not a word in the register table, and native code runs inside an unsafe foreign call where that register holds whatever the C code put there.
* Each `Pack` allocates once for everything it builds: a constructor's fields are already evaluated, so nothing can exit between the allocation and the last store.

#### Layouts

The constructor layouts are GHC's, so they are not hard-coded: they are learned by probing sample closures at startup ([`probeLayouts`](Layout.hs#:~:text=probeLayouts%20%3A%3A) in `Layout.hs`, into [`Layouts`](Layout.hs#:~:text=data%20Layouts)), because `UNPACK` only takes effect in optimized builds and the layout can differ between builds. The C side gets every tag and info pointer it writes from the same probes, never from a literal.

* A data constructor with 0 to 6 fields is one of `GEnum`, `GData1` … `GData6`, one object: the fields' boxed halves as pointers 0 to n-1, the type reference as the last pointer, then the packed tag and the fields' unboxed halves; 3 + 2n words, one load per field, the reference last so that field j is pointer j.
* Seven or more fields are a `GDataG` holding the two segment arrays directly, in reverse.
* A text is `GText rope`, a list `GList deque` and a bytes value `GBytes rope`: one object of two words each. GHC gives a type's first six constructors their own pointer tags and the seventh and later tag 7, so a value's shape is its pointer tag and, for tag 7, its info pointer.
* A data match ([`genDMatchClosure`](Codegen.hs#:~:text=genDMatchClosure%20%3A%3A)) dispatches on the shape: each arm's field count (which `DMatch` carries) fixes the shape of the constructor it is for, so the switch covers the shapes the arms have; a shape with one arm, in a match with no default arm of its own (one in which every constructor has an arm), branches straight to it, and a shape two or more arms share (`Either`'s two `GData1`s), or any shape when the match has a default (so a constructor without an arm may share it), loads the packed tag and switches on the constructor number, as the interpreter's `dataBranch` does; a data value of a shape no arm has runs the default arm, and anything else exits ([2026-10-21 match dispatch](benchmarks/2026-10-21-match-dispatch.md)). A lone wide constructor (`GDataG`, tag 7) always loads the tag: its info pointer only says "some type's wide constructor", so the arm compares the whole packed tag, and then the object's two array lengths against its count ([`requireWide`](Codegen.hs#:~:text=requireWide%20%3A%3A)), before reading (2026-10-24). A request match always loads the packed tag, which carries the ability number ([`loadPackedTag`](Codegen.hs#:~:text=loadPackedTag%20%3A%3A)), and checks the object's shape against the arm's count.

The first six constructors are the ones that seem to be most often used by Unison programs: `GList` 1, `GText` 2, `GEnum` 3, `GData1` 4, `GData2` 5, `GData5` 6 (base's `Map` node has five fields, and is a real constructor under the JIT since the `Map` replacement is off). 

`GData3`, `GData4`, `GData6`, `GDataG`, `GBytes` (bytes being rarer than texts), `GPAp`, `GCaptured`, records, `Foreign` and the rest share tag 7. The same reasoning fixes the order of the first six constructors of `GSection`, `GInstr`, `Prim1`, `Prim2` and `Foreign`, where it is the interpreter's own dispatch that benefits ([2026-10-15 constructor order](benchmarks/2026-10-15-constructor-order.md), [text and lists](benchmarks/2026-10-15-text-list.md)).

None of the orders is serialized or compared. Taking a constructor apart needs its field count, which comes from the object: the pointer tag for the shapes that have one of their own, the array lengths for a wide constructor. The match arm brings the count it was laid out for, and is taken only when the object has that many ([`genDMatchClosure`](Codegen.hs#:~:text=genDMatchClosure%20%3A%3A)).

#### Pointer tagging

Pointers to evaluated constructors carry the constructor number in their low bits. Native code must untag before reading fields and must store correctly tagged pointers when it writes into `bstk` or a constructor field. Every field of `GClosure` and `Val` is strict, so everything reachable from `bstk` is in weak head normal form and native code never has to evaluate anything; but a pointer taken from a stack slot may still be *untagged*: a top-level constant such as `noneClo` stored as itself puts the static closure's address there, which is untagged whether or not the CAF has been forced. GHC's optimized code reads a strict field without checking the tag, so native code checks the pointer tag before reading through such a pointer (a match on it sends tag 0 to the interpreter) and before storing it into a strict field ([`requireTagged`](Codegen.hs#:~:text=requireTagged%20%3A%3A): constructor and record packs, list literals and pushes, `Ref` and array writes), taking the slow path for tag 0. An evaluated CAF is an `IND_STATIC` to a `BLACKHOLE` whose indirectee is the value; the C helpers that follow indirections for the startup samples know that case.

#### The write barrier

GHC's collector is generational, and a minor GC doesn't scan old-generation objects unless it has been told they changed. `bstk` is a long-lived mutable array, so it will tend to end up in the old generation, and a pointer stored into it without telling the GC can leave a new object looking unreachable. Native code stores into `bstk` directly, with no barrier, and pays the barrier once per return to Haskell rather than once per write: no GC can run during the `unsafe` call, so the marks only have to be in place by the time native code returns. Before returning (on `OK`, an exit or an error), the C entry point sets `bstk`'s header to the dirty info pointer and marks every card (one byte per 128 elements) covering the slots native code could have written, from the frame it was entered with up to the high-water mark it kept in `Ctx` ([`unison_jit_mark_bstk`](../../../../cbits/jit_rt.c#:~:text=static%20void%20unison_jit_mark_bstk)). `ustk` holds no pointers and needs no barrier. Other mutable objects native code writes to get the same barrier compiled Haskell gives them: `Ref.write` calls the runtime's `dirty_MUT_VAR`, and `MutableArray.write` sets the array's dirty info pointer and marks the card. Marking the header and cards is sufficient on GHC 9.10.3: with it the debug runtime's heap sanity checks pass, and without it they fail. GHC's non-moving collector (`+RTS -xn`) is not supported, see [Non-goals](#non-goals-for-now).

#### The allocation budget

`allocate` takes blocks from the nursery, and once the nursery is empty it takes fresh blocks from the block allocator without limit; GC is only triggered when a *Haskell* heap check fails, and nothing native code does triggers one. So native code keeps an allocation budget, checked by the [poll](#preemption): after a budget exit, the interpreter's own allocation hits a heap check within a block or so, and the GC runs.

The budget is folded into the limit the fast path already compares against: `budget_end` is `hp` plus the words the budget still allows, and `hp_lim` is the nearer of the block's end and `budget_end`, so one compare covers both and the slow path finds out which it hit. When the budget is used up the slow path still hands out the object (nothing can exit halfway through a `Pack`), leaves `hp_lim` below `hp` so that the rest of the run's allocations take the slow path too, and the poll, which tests `hp` against `budget_end`, fires at the next function entry.

### Builtins and data structures

Most of what library code does is in builtins over a few representations: lists, text, bytes, numbers, arrays and refs. Two principles keep those native from end to end, [strict representations](#strict-representations) and [complete, checked ports](#complete-checked-ports). Which builtins are native is in [builtins.md](builtins.md). The parts:

- [Strict representations](#strict-representations) and [native implementations of builtins](#native-implementations-of-builtins): the two principles.
- [Where an operation runs](#where-an-operation-runs): inline, as an IR helper, or as a C helper.
- [The representations](#the-representations): lists, text and bytes, arrays, refs and pointers, numbers.
- [Hashing and comparison](#hashing-and-comparison): the universal operations.
- [Records](#records): structural records.
- [The IR helpers](#the-ir-helpers): C compiled to LLVM IR once, so one file serves every platform.
- [Startup checks](#startup-checks): every helper against the Haskell operation it ports.
- [The Haskell replacements](#the-haskell-replacements): base definitions the interpreter swaps for Haskell, and which stay swapped under the JIT.

#### Strict representations

The representations are strict in every field: a `List` is a strict finger tree ([`Deque`](../../../../../lib/unison-util-rope/src/Unison/Util/Deque/Internal.hs#:~:text=data%20Deque), held directly by [`GList`](../Stack.hs#:~:text=%3D%20GList)), a `Text` ([`GText`](../Stack.hs#:~:text=%7C%20GText), over [`Unison.Util.Text`](../../../../../parser-typechecker/src/Unison/Util/Text.hs#:~:text=newtype%20Text)) and a `Bytes` ([`GBytes`](../Stack.hs#:~:text=%7C%20GBytes)) are [ropes](../../../../../lib/unison-util-rope/src/Unison/Util/Rope.hs#:~:text=data%20Rope) of packed chunks on the same tree. So everything reachable from one is an evaluated constructor, and native code can take them apart and build new ones with no thunk in sight. The modules whose constructors the C side reads are compiled with `-O2 -funbox-strict-fields` in every build, so that the layouts don't change with the build's optimization level.

#### Native implementations of builtins

The native operations are complete ports of the Haskell ones, as C helpers that generated code calls directly, one file per family ([`jit_builtins_lists.c`](../../../../cbits/jit_builtins_lists.c), `_ropes`, `_text`, `_bytes`, `_arrays`, `_math`, `_compare`, `_records`), all included by [`jit_rt.c`](../../../../cbits/jit_rt.c).

`jit_rt.c` keeps the machinery (entering native code, allocation, the trampoline, the probes), so that the runtime is one translation unit and what the files share ([`jit_builtins.h`](../../../../cbits/jit_builtins.h): allocation, the small constructors, the list structure's internals) stays static and inlinable. 

Builtins are checked against the Haskell operations at startup on sample values (and by a longer randomized test on request); if anything differs, the JIT stays off. A helper that can't decide a case exactly answers "not handled" and the interpreter does it, so a helper is never wrong, only sometimes absent. The price is that a helper has to be kept in step with the Haskell operation it ports, which is what the checks are for. But given how rarely these implementations change, it seems okay. The current results match the interpreter's bit for bit.

#### Where an operation runs

An operation on a data structure is done in one of three places, by size. A few instructions (a wrapper check, a field load, a bounds check and a load) are generated inline, as arithmetic is; a few hundred lines that walk closures (the hash, the comparison walkers, the handler lookup, the record field lookup) are C compiled to LLVM IR at development time and compiled again by the JIT at startup ([the IR helpers](#the-ir-helpers)); everything else is a C helper the generated code calls by name.

Why do we sometimes pre-compile LLVM IR at development time? Well, a C helper never returns a two-word `(status, value)` pair: C and generated code would disagree on how such a pair comes back on the Windows x64 convention. So for a few builtins that need to return pairs, we write them in C and then compile to LLVM IR at development time, check in the IR, and dynamically compile at startup time.

#### The representations

- **Lists.** The deque is a strict finger tree: a prefix and a suffix digit of up to ten items as strict lists, a strict middle of nodes that are eight leaves inline or an array of two to eight children. Pushes and pops are amortized O(1) when a list is used once and O(log n) in the worst case; lookup, take, drop and append are O(log n). The C list helpers ([`jit_rt.c`, "Lists"](../../../../cbits/jit_rt.c#:~:text=%2F%2F%20Lists)) are ports of the module's operations: cons, snoc, uncons, unsnoc, lookup, take, drop, append, the views and splits that list patterns compile to, list literals.
- **Text and bytes.** The rope is the same finger tree with chunks for elements (a character count and a `Data.Text`, or a byte array) and sizes counted in characters or bytes. Invariants: no chunk is empty; two chunks next to each other hold more than a threshold (64) of elements between them, which bounds the number of chunks by the length. A chunk that a join made has slack: its array is over-allocated (starting at [`slackStart`](../../../../../lib/unison-util-bytes/src/Unison/Util/Bytes.hs#:~:text=slackStart%2C%20slackMax%20%3A%3A) bytes and quadrupling to 512), word 0 of the array holds the used length, and the chunk records the array's capacity; an append to the chunk that owns the array's tail (its end is the used length) advances the used length by compare-and-swap and writes in place, so building a text one piece at a time is one write per piece instead of a copy of the chunk, and of two appends to the same chunk the second copies ([`joinChunks`](../../../../../parser-typechecker/src/Unison/Util/Text.hs#:~:text=joinChunks%20%3A%3A), [`chunk_join`](../../../../cbits/jit_rt.c#:~:text=static%20StgClosure%20%2Achunk_join)). Every rope function in C ([`jit_rt.c`, "Ropes"](../../../../cbits/jit_rt.c#:~:text=%2F%2F%20Ropes)) takes a [`RopeKind`](../../../../cbits/jit_internal.h#:~:text=%7D%20RopeKind%3B) that holds the kind's constructors (the closure holding the rope among them, `GText`'s or `GBytes`'s) and whether counts are characters that have to be turned into bytes through UTF-8; the text helpers and the bytes helpers are the same functions with different kinds.
- **Arrays, refs and pointers.** Each array kind and a `Ptr` are `Foreign` wrappers around the unlifted array or the address; the layout probe reads each wrapper's info pointer and pointer tag. Sizes, reads and writes are generated inline ([`genArrayOp`](Codegen.hs#:~:text=genArrayOp%20%3A%3A), [`genPtrOp`](Codegen.hs#:~:text=genPtrOp%20%3A%3A)): the wrapper checks, the interpreter's bounds arithmetic exactly, then a `load` or `store` of the element's type, byte-swapped for the big-endian widths; a failed check branches to the call-out, so exactly the calls the interpreter rejects reach it, where it raises. New arrays get the RTS's layout (dirty info pointers, a zeroed card table, `allocatePinned` for pinned byte arrays); `Ref.cas` is `casMutVar#`'s compare-and-swap.
- **Numbers.** Floats are their IEEE bits in the unboxed slot. The generated code calls the libm functions compiled Haskell calls and uses the intrinsics for `sqrt` and `abs`; conversions to `Int` use the saturating intrinsic; `ceiling` and `floor` are GHC.Float's truncate-then-adjust; `atan2` is a C port of GHC's case-defined instance. The `--fast` build's interpreter differs on out-of-range conversions (the RULES that make them saturate don't fire at `-O0`), so the optimized build is the reference for edge cases.

#### Hashing and comparison

`Universal.murmurHashUntyped` is `Value.value` followed by a foreign call that hashes the reflected tree; the generator fuses the two into one call of [`unison_jit_murmur`](../../../../cbits/jit_helpers.c#:~:text=JitNat%20unison_jit_murmur%28int64_t), which walks the closures and feeds the same words to the same MurmurHash64A accumulator. A value it doesn't handle (a function, a map, a link, quoted code, a continuation, a big number) goes to the call-out.

`Universal.==`, `<`, `<=`, `>`, `>=` and `compare` are generated inline for two numbers of the same type (and two chars or floats for equality); every other pair goes to [`unison_jit_ueq`](../../../../cbits/jit_helpers.c#:~:text=JitNat%20unison_jit_ueq%28int64_t) or [`unison_jit_ucmp`](../../../../cbits/jit_helpers.c#:~:text=JitNat%20unison_jit_ucmp%28int64_t), IR helpers that walk both values and give the interpreter's [`Eq Val`](../Stack.hs#:~:text=instance%20Eq%20Val) and [`Ord Val`](../Stack.hs#:~:text=instance%20Ord%20Val) answer, quirks included (`jit_helpers.c` spells them out: a `Nat` equals an `Int` of the same value, ordering compares constructor indices only while equality compares the whole packed tag, the fields of an `Any` compare with their types, floats order by their bits so `-0.0` and `0.0` are equal under ordering but not under equality, different closure kinds order as [`closureNum`](../Stack.hs#:~:text=closureNum%20%3A%3A)). "Not handled", and so the interpreter's call-out, for functions of different references (comparing two hashes), maps, two foreign values of different kinds (the interpreter raises) and anything unevaluated. Startup runs about fifty pairs of every kind through the C copies and the Haskell instances ([`probeCompare`](Layout.hs#:~:text=probeCompare%20%3A%3A)), including pairs that must be declined.

#### Records

A record is `GRecord shape vals` ([`RecordShape`](../MCode.hs#:~:text=data%20RecordShape): the interned shape reference, the field names, and the slot of each interned field name as two unboxed arrays, so a field lookup is a binary search; the values a `Vector Val` with slot i holding field i). Both are flattened into the closure by `-funbox-strict-fields`; pointer tag 7, told from `Foreign` by its info pointer. The elements of the values' array are evaluated `Val` boxes, which the interpreter builds so ([`recordVals`](../Stack.hs#:~:text=recordVals%20%3A%3A)) because native code reads them. `RecPack` ([`genRecPack`](Codegen.hs#:~:text=genRecPack%20%3A%3A)) allocates the record, its array and the boxes in one chunk, copying the shape's words from the pool's empty record of that shape; `RecUnpack` ([`genRecUnpack`](Codegen.hs#:~:text=genRecUnpack%20%3A%3A)) asks [`unison_jit_rec_pos`](../../../../cbits/jit_helpers.c#:~:text=int64_t%20unison_jit_rec_pos), an IR helper, for each field's slot in the *value's* shape, since one pattern or getter runs on records of every shape its row type admits; `RecSet` is a C helper that allocates the copy with one slot replaced. Anything that isn't a record, a field the value hasn't, or an untagged element is the slow path. The comparison walkers and the hash handle records like data ([2026-10-15 positions](benchmarks/2026-10-15-positions.md), [records](benchmarks/2026-10-15-records.md)).

#### The IR helpers

A few helpers that walk closures (the hash, the comparison walkers, the handler lookup, the record field lookup) are hot enough that a call into C costs: generated code calls them as LLVM IR instead, so that LLVM can inline and specialize them. They are written in C in [`cbits/jit_helpers.c`](../../../../cbits/jit_helpers.c), which is compiled into the runtime like any other C (the copies the startup self-checks run, and the fallback under `UNISON_JIT_DISABLE=irhelpers`), and also, at development time, by clang into [`cbits/jit_helpers.ll`](../../../../cbits/jit_helpers.ll), with everything target-specific stripped out ([`gen-helpers.sh`](../../../../cbits/gen-helpers.sh) and [`gen-helpers.hs`](../../../../cbits/gen-helpers.hs), whose comments say what goes and why). The `.ll` is committed, embedded in the executable ([`Helpers.hs`](Helpers.hs)) and added to the JIT at startup as a module of its own; the point is that the native code differs per platform but the IR doesn't, so one file generated on macOS or Linux serves every target. The module costs about 55 ms once, nothing per call ([2026-10-10 helpers](benchmarks/2026-10-10-helpers.md)). After any change to `jit_helpers.c` or `jit_internal.h`, `gen-helpers.sh --check` says whether the `.ll` is in step.

Two consequences. The file may contain only a restricted C ([`jit_internal.h`](../../../../cbits/jit_internal.h) says what): fixed-width types, no varargs, no stdio, and calls only into the tables and walkers `jit_rt.c` exports for it. And nothing is resolved from the process's own symbol table (Linux doesn't expose an executable's symbols to `dlsym` without `-rdynamic`, Windows not at all): every C function or table that generated code or a module refers to is listed by name and address in `jit_rt.c` ([`unison_jit_helpers`](../../../../cbits/jit_rt.c#:~:text=static%20const%20JitHelper%20unison_jit_helpers), [`unison_jit_table_addresses`](../../../../cbits/jit_rt.c#:~:text=int64_t%20unison_jit_table_addresses%28const%20char)) and defined in the JIT as an absolute symbol before the first module goes in; only the C library's functions (`memcpy`, `sin` and the rest) come from the process, and those are in shared libraries on every platform.

#### Startup checks

The constructor layouts are GHC's, not ours, so they are checked at startup like the closure layouts (about 35 ms in all on the optimized build; `UNISON_JIT_LOG=1` reports each): the helpers learn the constructors from sample values, a C walk checks the shape and the stored counts of every constructor in a set of samples, and each helper is then run against the Haskell operation it stands in for. If anything differs, the JIT stays off. `UNISON_JIT_STRESS=lists=N`, `texts=N`, `bytes=N` run N random operations through the helpers, each on the results of earlier ones, checking every result's structure and elements against Haskell. The linker drops C functions that nothing refers to, and only generated code refers to the helpers, so the runtime keeps a table of them.

#### The Haskell replacements

The runtime swaps some library definitions for Haskell as it loads code, by hash ([`functionReplacementList`](../Foreign/Function.hs#:~:text=functionReplacementList%20%3A%3A)): base's `Map` type becomes `Foreign (WrapMap m)` around a `Data.Map Val Val` (its constructors become foreign functions, `dataBranch` emulates a match on the wrapper, [`formDataReplaced`](../Stack.hs#:~:text=formDataReplaced%20%3A%3A) builds the wrapped form when a value is reified, and reflection turns it back), and some functions become foreign calls: the `Map`, `Set` and `Multimap` operations, `List.range`, `List.sort`, the JSON serializer and parsers, `Avro.decodeBinary` and the `Bytes.read*` family. In the interpreter each is a win over the Unison code it replaces. Under the JIT each is a call-out, the round trip on top of whatever the operation does, and the generator never sees the Unison code, so nothing around the call is compiled with it either.

The list is therefore applied through one setting fixed at startup ([`replacementActive`](Config.hs#:~:text=replacementActive%20%3A%3A)), the one setting that is not carried in the JIT handle, since the value decoder and the foreign function table that consult it have no handle in reach and the two must agree across the process: with the JIT off every entry, as before; with it on, the entries tagged `Always`; `UNISON_REPLACEMENTS=on|off` forces all or none, for measuring and as the escape hatch for a program that was fast only because of one. A compiled program's MCode has the replacements of the mode it was compiled under baked in, so its file carries that mode and `runStandalone` sets it before anything decodes a value ([`setReplacements`](Config.hs#:~:text=setReplacements%20%3A%3A)); the environment of the process running it does not decide (2026-10-24). The `Map`, `Set` and `Multimap` functions take and return the replaced type, so they share the type's tag ([`mapFamily`](../Stack.hs#:~:text=mapFamily%20%3A%3A)). The two forms never mix inside a process, and the serialized form of a map is the same interchange node either way, so the bytes of a serialized map, and with them `Universal.murmurHash`, depend on neither the setting nor the tree's shape; processes with different settings interoperate.

The tags were decided by measuring each replacement against the compiled Unison code at three sizes ([2026-10-11 replacements](benchmarks/2026-10-11-replacements.md)): the `Map` family and `List.range` go native (the `List.range` rows of that day didn't force the result and are wrong; measured with the result forced, native is faster at 10 and at 100000 elements and 25% slower at 1000, [2026-10-21 List.range](benchmarks/2026-10-21-list-range.md)); `List.sort`, the JSON functions and `Avro.decodeBinary` stay replaced (2.5 to 20× faster as call-outs than base's Unison code compiled); the `Bytes.read*` family stays because the generator has native code for those foreign functions, so the replacement costs nothing.

### Ability requests

A handler whose cases each either never resume the continuation or resume it once, in tail position, with the same handler (`handle k v with h s'`) is *affine*. Such handlers are most of the handlers in practice (state, exceptions, streams), and their requests need no continuation capture, so they can run natively; everything else about abilities stays the interpreter's. The parts:

- [Affine handlers in the interpreter](#affine-handlers-in-the-interpreter): the second matcher and the environment.
- [What runs natively](#what-runs-natively): the request, the matcher, and the two instructions.
- [The environment in the context](#the-environment-in-the-context): how native code finds the handler.
- [The `InLocal` rule](#the-inlocal-rule): when an environment switch can be dropped.
- [The switch](#the-switch): when it can't.

#### Affine handlers in the interpreter

[`ANF/Optimize.hs`](../ANF/Optimize.hs#:~:text=affinePreOptimize) recognizes the shape and adds a second matcher to the handler's group, which the `handle` installs when no non-affine handler is in force. The affine matcher is an ordinary function of the handler's state, an `Affine` closure ([`GAffine`](../Stack.hs#:~:text=%7C%20GAffine): the ability numbers, the environment at installation, an `IORef` holding the current handler) and the request. A request is `Pack` of the request constructor then `App True (Dyn n)`: the interpreter looks `n` up in the environment ([`HEnv aenv denv`](../Stack.hs#:~:text=data%20HEnv): `denv` maps numbers to non-affine handlers, `aenv` to the refs of affine ones, `denv` shadowing `aenv`), reads the ref, and calls the closure with the request as its argument and the requester's continuation unchanged. The case then runs `InLocal` (switch to the handler's own environment, with a `Local` frame to switch back), builds the handler for the next request (`Name`), stores it in the ref (`SetAff`), and `Yield`s the request's result straight back to the requester. Nothing is captured.

#### What runs natively

The request: [`genApp`](Codegen.hs#:~:text=genApp%20%3A%3A)'s `Dyn` case resolves the handler and makes the call through the closure-call code, in a `Let` binding or in tail position, so the handler's frame and continuation are what they would be for any function value. The matcher: `RMatch` is a data match ([`genRMatch`](Codegen.hs#:~:text=genRMatch%20%3A%3A)) whose tag word is `ability << 16 | constructor`, zero for the pure request; its arms push the constructor's fields with the arities of the ability's declaration, which the generator finds by number. `SetAff False` is a call of [`unison_jit_set_aff`](../../../../cbits/jit_rt.c#:~:text=int64_t%20unison_jit_set_aff%28UnisonJitCtx%20%2Actx%2C%20void%20%2Aaff%2C%20void%20%2Av%29%20%7B), which writes the new handler into the ref through the runtime's barrier. `InLocal` is a no-op under the rule below, and a real switch otherwise. Everything else about handlers stays the interpreter's: `Reset` (the `handle` itself), `Capture`, `Discard` (the abort cases), `SetAff True` (the environment update), and the non-affine matchers. A request whose number the environment maps to a non-affine handler, or to nothing, exits at the `App`. The estimate counts a request as a call of a function value and `InLocal` and `SetAff False` as native instructions, so affine matchers and request loops pass the rule; `UNISON_JIT_DISABLE=requests` turns all of it off.

#### The environment in the context

The trampoline writes the `HEnv` it was entered with into the first slot of the boxed stack's out-area and copies it into `ctx->henv`; when it differs on return, the current one is written back there, so the pointer is a collector root while Haskell runs around the call and native code holds one only during the call. `ctx->henv` is the *current* environment: every request site reads it, and an `InLocal` switches it. The lookup, [`unison_jit_resolve_dyn`](../../../../cbits/jit_helpers.c#:~:text=void%20%2Aunison_jit_resolve_dyn), is an IR helper: it walks `denv` (a hit there is "not handled") and then `aenv`, visiting the nodes of the `IntMap` (a walk that compares keys at the tips is independent of the prefix encoding, which changed in containers 0.7, and the maps hold a handful of handlers), takes the `STRef` box at the tip, and returns the `MutVar#`'s content, or null for anything that isn't the expected shape. Untagged pointers along the way are settled; an unevaluated thunk is left to the interpreter. Startup ([`probeEnv`](Layout.hs#:~:text=probeEnv%20%3A%3A)) builds sample environments, hands them to the C side to learn the constructors, and runs the lookup against known answers; a failed check keeps the JIT off.

#### The `InLocal` rule

The environment switch is observed by requests, `Reset`, `Capture`, `Discard`, `SetAff True`, raising an exception, and by any exit that resumes the interpreter ahead of one of those. So `InLocal` is dropped ([`localUnobserved`](Codegen.hs#:~:text=localUnobserved%20%3A%3A)) when the rest of its case, up to the `Yield`, is straight-line code made of literals, constructors and records, partial applications, `SetAff False` and the native primitives. Even an exit inside such a body is harmless: the interpreter finishes the case in the requester's environment, which nothing left in it reads, and the `Yield` pops the requester's frame, which is what the `Local` frame would have left it with.

#### The switch

For a case that observes the switch (one that calls a function or makes a request of its own), [`genInLocal`](Codegen.hs#:~:text=genInLocal%20%3A%3A) calls [`unison_jit_local_env`](../../../../cbits/jit_rt.c#:~:text=void%20%2Aunison_jit_local_env%28UnisonJitCtx%20%2Actx%2C%20void%20%2Aaff%29%20%7B), which checks the closure is an `Affine` and bump-allocates the case's `HEnv`, as the interpreter builds it; the generated code keeps the old environment in a register, stores the new one in `ctx->henv` and counts the switch. The rest of the case is generated with the switch in force, which means three things. Its `Yield` stores the old environment back and uncounts the switch ([`endLocal`](Codegen.hs#:~:text=endLocal%20%3A%3A)) before returning, so whoever called the handler sees the environment it had. A tail call at that depth would leave the switch in force with nothing to end it, so it is an exit instead. And every exit or unwind in between writes a `Local` frame record: a record with a negative index, placed among the function's records where the switch was made, with the old environment stored in the boxed out-area where the collector sees it; the trampoline turns it into a `Local` frame, so everything after is as the interpreter would have left it after its own `InLocal`, and the re-entry function after a call-out inside the case is generated without a switch. The out-area has room for 32 environments, so the switch takes its slow path, a call-out where the interpreter runs the `InLocal` itself, when that many are in force ([2026-10-11 requests](benchmarks/2026-10-11-requests.md), [local](benchmarks/2026-10-11-local.md)).

### The constant pool

Native code needs some Haskell heap objects: the `Reference` written into every constructor by `Pack`, the type-tag closures for unboxed values, constructors without fields, `GCombInfo` for building partial applications, `Lit` values, pre-evaluated top-level values, and empty records by shape. These objects move on every GC, so their addresses can't be baked into machine code. Instead there is a *constant pool* ([`Pool.hs`](Pool.hs#:~:text=data%20Pool%20%3D)): a `MutableArray Closure` owned by Haskell, whose address is passed to native code on every entry, exactly like `bstk`, through `Ctx`. Generated code refers to constants by pool index ([`poolIndices`](Pool.hs#:~:text=poolIndices%20%3A%3A) assigns them per module; [`closureFor`](Pool.hs#:~:text=closureFor%20%3A%3A) builds each entry from its [`PoolKey`](Pool.hs#:~:text=data%20PoolKey)). Addresses are stable for the duration of an `unsafe` call, so this is safe for the same reason reading `bstk` is. The pool's first entries are fixed: the type-tag closures and the boolean constructors, at indices every function may use without a check.

There is one pool per process rather than one per module, because native code in one module calls into another without passing through the trampoline, so there would be no moment to switch pools. Entries are interned by key when a module is compiled, so two modules using the same `Reference` share an index. The array grows by copying, and superseded arrays are kept alive so that an address read at entry stays valid for the rest of that native run. Because code is installed while native code runs, a run can call a function that is newer than the array it holds. So a new entry is also written to the superseded arrays where it fits, and a function that uses an index beyond the size of the pool's first array checks the size of the array it was given at entry; if it is too small, it exits and is entered again with the current array.

### Alternatives considered

Things that were proposed along the way and not done, so that they aren't proposed again without the reason being known:

- **A dynamic safety net instead of the static estimate.** Count a function's exits against its entries at run time and drop its native code when it exits nearly every time. It can't tell a function that is all call-outs from one that does real work and then calls out once, and it compiles the code it later rejects. The static estimate reads the code, needs no bookkeeping, and never compiles what it refuses; what it can't see is behaviour (which branch runs, whether a function value is native), and the cheaper round trip made that blind spot cost little.
- **Re-entry points as one function with an entry-index switch.** Every `Let` boundary on the fast path would become a merge point, which hurts LLVM's optimization of the fast path, and the entry branch would be shared between normal calls and re-entries. Separate functions, generated when used, keep the fast path straight.
- **Static analysis of which re-entry points are reachable.** Nearly all are: any callee can exit at its poll, its stack check or the C stack guard, so the analysis would remove almost nothing. Whether a point is *used* is dynamic, so it is counted.
- **Not counting ad hoc exits at all,** instead of [capping](#re-entry-functions-on-demand) the re-entry functions they ask for. A long-running invocation interrupted by the timer would then run the rest of itself interpreted every time, with no way back into native code until it returned. The cap keeps the way back for the few Lets where the time is spent.
- **Native code on a C stack of its own,** so that a call-out switches stacks, lets the interpreter run one instruction, and switches back with every native frame still there. The GC can run during that instruction and move objects, so every parked frame would have to put its live values on the Unison stack before the call and reload them after, which is most of the cost of unwinding, plus a parked stack per Unison thread and per nested evaluation, and exceptions would have to discard it. Set aside in favour of making exits rarer and cheaper; worth revisiting only if exits still dominate.
- **A bare code pointer in a `CallOut` exit** instead of a cell. The re-entry function can itself exit with `GrowStack` or `Reenter`, both of which mean "call this function again", and the trampoline does that through a cell.
- **Stack maps for write-back** (`llvm.experimental.deoptimize`, `gc.statepoint`) instead of passing the live slots to a routine. LLVM lowers `deoptimize` as a call that never returns, so the runtime routine would have to pop the native frame and restore its callee-saved registers itself, in assembly per platform; a `statepoint` returns normally but spills the operands at the site just as the call does, plus a stack map to parse.

## Further reading and references

Again:

* How to turn it on, what it needs and what the settings mean is in [usage.md](usage.md)
* How to build, run and test it is in [development.md](development.md)
* Which builtins are native is in [builtins.md](builtins.md)
* Ideas not done yet are in [optimization-ideas.md](optimization-ideas.md)
* Benchmarks from along the way in [benchmarks/](benchmarks/)

And background the design draws on, and related work:

* [LLVM ORC v2](https://llvm.org/docs/ORCv2.html), the JIT API used, and the [LLVM language reference](https://llvm.org/docs/LangRef.html).
* GHC's runtime, as documented in the GHC commentary: [heap objects](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects), [pointer tagging](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/pointer-tagging) and the [garbage collector](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc); the tagging scheme is from Marlow, Yakushev and Peyton Jones, [*Faster laziness using dynamic pointer tagging*](https://simonmar.github.io/bib/papers/ptr-tagging.pdf).
* Exiting to the interpreter mid-function and rebuilding correct interpreter state from the compiled function's state is the same sort of idea as what JavaScript engines call an OSR exit or deoptimization. See Filip Pizlo's [Speculation in JavaScriptCore](https://webkit.org/blog/10308/speculation-in-javascriptcore/) (2020), in particular its section on OSR exit or Benedikt Meurer's [An introduction to speculative optimization in V8](https://benediktmeurer.de/2017/12/13/an-introduction-to-speculative-optimization-in-v8/) (2017). Tracing JITs do something similar at a *side exit*; see [*Trace-based just-in-time type specialization for dynamic languages*](https://dl.acm.org/doi/10.1145/1543135.1542528) (2009).