# JIT: optimization ideas

Ideas not done yet, and ideas tried and not kept. What is implemented is described in
[design.md](design.md) and the code, not here, and the numbers
behind every decision are in [benchmarks/](benchmarks/README.md). An entry here says what the
change would be and roughly what it would buy; when one is done it leaves this file.

## Unimplemented

### Static enums

Right now, an `Enum` / nullary constructor is three words that never change; like GHC's
own nullary constructors, one per (type, constructor) could be shared instead of allocated
at every `Pack`. The constant pool already holds one per `Pack … ZArgs` site for native code; the
interpreter's `buildData` would read a per-cache table of them.

### Faster pattern matching

Right now, pattern matching has to do two levels of matching, first, to determine
which arity constructor it is, and then to branch on that constructor's tag. We could
either: 

* Switch back to the generic DataN representation for everything, which lets you skip the first level of matching to determine if it's a `Data1`, `Data2`, etc, then still needing to match on the constructor number. This has the downside that it uses more memory per closure since you have to store two arrays, boxed and unboxed, for the constructor's arguments. And copying an array is a bit slower than manually copying a couple words. Though I don't know that we have benchmarks showing this exact combination.
* Have more `Closure` constructors, one for each (constructor number, arity) combination (perhaps z-ordered or using hilbert curve?), so that both the interpreter and native code can gain information about both with a single match. 

If it helps, I don't think we need more than 16 bits for the constructor arity or for the constructor number. 

### One allocation for several `Pack`s

Consecutive `Pack`s in a straight-line run, and the arithmetic between them, could share one
allocation. The batch boundary is the next instruction that can exit (a `DIVN`, a foreign
call), since an exit between two `Pack`s would leave the later objects unwritten in the
nursery. Worth less since allocation became an inline bump: batching now saves only a compare
and a branch per object.

### The Unison stack as a linked list of chunks

`ustk` and `bstk` are single arrays, so growing the stack is a reallocation and a copy of
everything below `sp`, and native code pays for each growth with an unwind of every native
frame. GHC's own threads haven't had a copying stack since 7.2: a linked list of fixed-size
chunks, an overflow links a new one in with the top kilobyte copied across for slack, growth
is O(1). The same shape for the Unison stack would make growth cheap for the interpreter and
remove the unwind for native code, since a function whose frame doesn't fit could keep
running in a new chunk. What it costs: slot addressing assumes a contiguous frame, so a frame
would have to live in one chunk (the maximum frame size is known, so an entry check can
guarantee it), and everything that walks the stack across frames (`Capture`, `saveFrame`'s
copies, continuations, the GC marking in `unison_jit_enter`, the C helpers reading argument
slots) would follow chunk links. A sizeable interpreter change.

### Wide stack slots

Idea is to store arity <= K data constructors unboxed on the stack. K=1 or 2. 
So if you write a function that returns an `Optional Nat`, there's no allocation
that happens, even without inlining.

If K=1, then each stack slot becomes 2 words wide. First word is the constructor tag,
second word is the constructor argument, similar to now. This second word will
go unused very often, but that might be fine.

If K=2, then each stack slot becomes 3 words wide, etc.

### Escape analysis + scalar replacement

This needs some research. Just doing escape analysis alone seems like less of a win
than you'd think. Like if you just convert heap allocation to stack allocation, that
doesn't matter and can even be slower, since allocation is bumping a pointer and 
GC doesn't traverse garbage in the youngest generation.

But if combined with other optimizations, you can start doing interesting stuff. 

The thing I don't like about it is it's sensitive to inlining, which makes it
somewhat fragile (compared to the wide stack slots idea, which is robust and
easy to think about).

### Dropping the reference pointer from data closures

The packed tag carries the type number, so the reference field is redundant: one word and
one traced pointer per value. It stays because `compareClosure`'s different-types path
(inside `Any`) orders by reference, and that path is reached from `Data.Map`'s own calls on
the replaced map through the `Ord Val` instance, which can't take a lookup function. A map
parameterized by a comparison would free it (`data-layout-plan.local.md`).

### Caching compiled code across runs

Definitions are content-addressed, so a hash of a module's definitions is a cache key, and
LLVM's JIT takes an object cache. On a program's second run the compile time would be near
zero. The obstacle is that generated code bakes in run-specific values: exit and frame table
indices, constant pool indices, and the addresses of native cells. Those would have to become
indirections or relocations before an object could be reused. The one change that would make
compile time stop mattering rather than smaller.

### Specialization on function arguments

A call to `foldLeft` with `(Nat.+)` could produce a specialized version of `foldLeft`, with
`(Nat.+)` effectively inlined into the body; a hot `List.map f` could get its own copy with
`f` inlined, which removes the indirect call altogether. The call to a function value is
the one call native code can't inline (it goes through the cell in the `GPAp`), so a loop
over a closure pays an indirect call per iteration even when the closure is native.

### Tiered compilation

Compile the trigger alone first, in a small module, and the batch later with direct calls,
replacing the cell pointers: cuts the latency of a hot loop running interpreted while its
batch compiles, and separates that latency from batch quality (a bound on a module's size in
IR, rather than in definitions, becomes possible). The same shape serves compile time: a
cheap first tier (no passes, the cheapest backend) and a recompile of what turns out hot
with the full pipeline. [2026-10-09 O0](benchmarks/2026-10-09-o0.md) measured the cheap tier
at several times cheaper to produce and 5× slower to run, and found that full O0 with workers
crashes (fast instruction selection on `tailcc` and `musttail`), which would need fixing
first.

### Several compile threads

LLJIT can compile on several threads (`LLVMOrcLLJITBuilderSetNumCompileThreads`). Total CPU
wouldn't change, but a hot function would get its native code sooner when the queue holds two
or three big modules. Only worth it if latency to native code is what programs feel; the
compile-thread wake-up benchmark can tell.

### Raise the inline threshold

With exits as one call per site, the inliner charges for the call at each site where a branch
to a shared block was free, and one worker on the suite (murmur's, 16 sites) stopped being
inlined: cost 240 against LLVM's threshold of 225, the row 3% slower.
`UNISON_JIT_INLINE_THRESHOLD=300` recovers it with no other row or the IR size moving,
[2026-10-09 exit call](benchmarks/2026-10-09-exit-call.md). A global inliner setting affects
every module, so it is left at the default until there is more to decide it on.

### Build IR without text

The generator builds each module as Haskell `String`s, megabytes of them, which is most of
what the compile thread allocates, and that allocation is shared with the running program
(more GCs, major ones sooner). A builder over byte arrays, or LLVM's C API or bitcode, would
cut it. Not for the parse: that is about 1% of a module's compile time
([2026-10-09 O0](benchmarks/2026-10-09-o0.md)).

TODO: is this still relevant?

### Ideas for calling convention improvements

- Re-entry functions pass everything through the Unison stack. One that is hot (a loop that
  exits every iteration re-enters every iteration) could get a worker-like body.
- Functions that return several values keep the uniform form.
- Uniform functions compare `ap` against `fp` before every return; workers moved that check
  into the wrapper, and the uniform form could check once at entry too.
- The type-tag closures are loaded through the pool at entry (two loads each); they could be
  fields of `Ctx`.
- A function's own cell is loaded before every self non-tail call; once at entry is enough.
- The continuation of a call-out with no fast path is generated with the function; it could
  be counted and generated on demand like the other re-entry points.
- A frame nested inside k inline `Let` bindings writes k + 1 records on unwind; fine unless
  unwinds turn out to be frequent.
- A leaner entry for leaf functions, for the ones the "too small to enter" verdict keeps
  interpreted today.

### Partial applications of function values

A partial application is built natively when the function is known at code generation. A
function value applied to too few arguments (`g = f x` where `f` is itself a closure) still
resumes in the interpreter: whether the call is under-saturated is only known at run time, so
the closure-call code needs a second continuation that joins the body with the new closure
instead of a call's result. The helper already handles closures with captured arguments.
Also: more than four arguments at once.

### Ensure no thunks on the boxed stack

Make the interpreter guarantee that a boxed stack slot only ever holds an
evaluated, tagged pointer, instead of native code checking the tag on every store into a
strict field. See `bpoke`, `bpokeOff` and `poke` in `Stack.hs`. A thunk causes
an exit from native code, since native code won't evaluate Haskell thunks.

There were a couple places where the interpreter was missing an evaluation (fixed now), 
but also Haskell CAFs remain as untagged pointers, and Haskell let-floating optimizations can produce
CAFs when you don't expect them. A fix is `evaluate` in `bpoke` and friends, storing the result
rather than the pointer handed in (for a CAF, the indirectee), or making such constants
genuinely static literals; either way, grep the `bpoke` call sites for other `Closure`
constants stored directly.

Cost: a tag test and an untaken branch per boxed store, to be measured on the suite (it may
even help the interpreter, since nothing downstream enters a thunk on read). What it buys: a
real invariant that is simpler than "every native write checks", covers places not
instrumented, and makes the read-side guard in `genDMatchClosure` stop costing a resume for a
`None` from a call-out. Plan: disable `requireTagged`, confirm the old reproducers
(`> Bytes.at 0 0xs`, `> Text.uncons "abc"`) crash again, land the `bpoke` change, confirm
they pass, re-enable the checks as a safety net, measure both the interpreter and `jitSuite`.

### A rewrite pass before code generation, starting with bytes literals

The parser turns `0xsdeadbeef` into `Bytes.fromList [222, 173, 190, 239]`, so a bytes
literal compiles to a list literal of n boxed Nats and a conversion at every evaluation,
where the desired code is a `Bytes` constant in the pool, one load. There is no literal form
for it: `ANF.Lit` and `MLit` have numbers, chars, text and links but not bytes, so the
rewrite needs a new `Lit`/`MLit` case (which touches the code serialization and its version)
or a JIT-side pseudo-instruction, with the list-and-call kept for the interpreter. The
pattern is easiest to recognize on ANF, before `emitSection` assigns slot indices; at the
MCode level removing the bindings means renumbering everything after them.

Keep such rewrites modular and separate from codegen: a pass, or a small framework of
rewrite rules, over ANF or MCode that the compiler runs before emitting, so that further
rules accumulate there without each being threaded through `Codegen.hs`. Later
candidates for the same pass: constant folding of arithmetic on literals, `Text` appends of
literals, `List` literals of constants as pool values. With a real `MLit` case the
interpreter benefits too.

### Worker-wrapper optimizations

Standard optimizations to avoid having to repeatedly construct and deconstruct,
especially during loops.

If you have a function that pattern matches and is called recursively, have
a separate worker for each constructor. The worker takes an unboxed sum, and
the caller doesn't bother allocating a full data type and pattern matching, 
it just calls the correct worker with an unboxed sum. Works for any number
of constructors, and the workers can be inlined or not.

### The rope

`Unison.Util.Rope`, a finger tree of chunks ([the rope benchmark](benchmarks/2026-10-02-rope-vs-old-rope.md)).
Open: appending two large texts is slower than on the size-balanced tree it replaced (150 to
290 ns against 20 to 30 when both sides have a middle), which is the Deque's append and
improves with it; a chunk whose character count equals its byte length is all ASCII and needs no scan
to index, take or drop, which the chunk instances and the C helpers (`utf8_prefix`) could
check; `take` and `drop` build the part before the cut and then `snoc` the cut piece,
allocating the top level twice; equality and comparison in C find each next chunk by
position, a walk down per chunk, where a cursor would make them one pass. Any change here
has to be made in the C helpers too, which the startup checks and `UNISON_JIT_STRESS=texts=N`
enforce.

### Lists

`Unison.Util.Deque`, a strict finger tree ([the Deque benchmark](benchmarks/2026-10-02-deque-vs-data-sequence.md),
[in the suite](benchmarks/2026-10-02-native-lists-in-the-suite.md)). Open on the native
side: every operation is a call into C, including push and pop, which are a dozen
instructions and could be generated inline for the common cases (the digit has room; the
digit keeps an item) the way byte array reads and writes are (`byteArray` in `Codegen.hs`,
[measured](benchmarks/2026-10-10-helpers.md)); `List.size`, `Text.size` and `Bytes.size`
are a field load or two by the same recipe; a list literal is one call per element plus one to wrap, where a
helper taking the elements from the stack would build the tree in one pass; `SPLL`/`SPLR` do
a take and a drop, two walks to the same place; the list operations that are foreign
functions (`List.sort`, conversions from `Text` and `Bytes`) are call-outs.

Open in the structure: `append` always extends the left prefix, so `acc ++ [x, y]` on a
short list copies `acc`'s cells, where the rope chooses the shorter side (a few lines there
and in `lv_append`); it trails `Data.Sequence` on `append` of two large pieces (1.5 to 1.8×),
`drop` (1.1 to 1.3×) and `fromList` of a long list (3×, a fold of `snoc`; the bulk
`fromListN` measured no faster for a reason not found), spending the time reading digit lists
cell by cell, so digits that are one object (an array with an offset so pops don't copy) are
the thing to try. (A deque of two skew binary lists back to back, `Unison.Util.Skews`, was
the first attempt and was never measured; it was deleted on 2026-10-19 and is in the history
before that date if the idea comes up again.)
Any change has to be made in the C helpers too (`UNISON_JIT_STRESS=lists=N`).

### `handle` itself

Lower priority: `Reset` runs once per evaluation of a `handle`, affine or
not, and only shows when the `handle` itself is in a hot loop; the ceiling is a few hundred
nanoseconds per iteration. `Reset` with an affine handler needs a `MutVar#`, an `Affine` closure and an extended partial
application (allocations of known layouts), an insert into `aenv` (building `IntMap` nodes
natively, or a helper), an `AMark` record kind, and the pure-case dispatch when the thunk
returns (a closure call to the handler with `Data1 effectRef 0 v`). Worth it when `handle`
runs per iteration: 445 ns per `handle` with two requests on the suite's row, against 35 ns
per request. `Discard` (abort cases, caught exceptions) is a non-local exit and stays one.

### Open measurements

- `Map.intersect` and `Map.==` are 10 times faster as call-outs to the Haskell `Map` than
  native (15 against 190 ns, 28 against 300), and `fromList` of sorted input twice as fast;
  the family is tagged as one. Tagging per function would need the two representations to
  convert at the boundary, which is what the family tag avoids.
- Private copies on a program where a hot loop calls small library functions from other
  definitions, which is where they should show; the suite's steady-state rows didn't move
  ([2026-10-04](benchmarks/2026-10-04-private-copies-and-reentry-batches.md)).
- The allocation budget with a small nursery (`+RTS -A256k`) or on a capability shared with
  other busy Haskell threads; it relies on the interpreter's next heap check to trigger the
  GC after a budget exit, and has only been tried with the defaults.
- `Json.toText` is 20% slower than on the old rope and the cause is not found
  ([what was tried](benchmarks/2026-10-02-text-and-bytes-helpers.md)); the interpreter's
  `catt` path is the place to look, since the same `"[" ++ x ++ "]"` is three times faster
  through the C helper.
- (Resolved 2026-10-17.) JSON parsing and `List.range` per element looked slower than on
  `Data.Sequence`, which was lazy in its elements: those rows ignore their result, so trunk
  never built the JSON values' children or the range's numbers, and the strict Deque does.
  With the result consumed the two cost the same; `benchmarks/2026-10-16-against-trunk.md`.

### Fallback: tail-call the body function at the end of an inline binding

If re-entry functions on demand turn out not to be enough, an inline binding could end with
a `musttail` call to the body function instead of having the body inlined after it, so every
body exists once. It costs the fast path its registers: the binding's results and every live
slot travel through the Unison stack at each `let`, as the interpreter does, and LLVM can no
longer optimize across the boundary. The case it is for has been measured: with a threshold
of 1, so that every re-entry point met is generated, the benchmark transcript spends 8.5 s
compiling against 0.8 s normally.

### Call-outs without unwinding: native code on its own C stack

An exit in a callee unwinds every native caller, and each comes back through a re-entry
point. If native code ran on a C stack of its own, a call-out could switch back to the
Haskell thread's stack, let the interpreter run the one instruction, and switch back in with
the native frames still there. The obstacle is the GC, which can run during that instruction
and move objects: every parked native frame would need its live values on the Unison stack
before any call and would reload them afterwards (a flag set by the trampoline could make the
reload conditional). There would be a parked stack per Unison thread and per nested
evaluation, and exceptions thrown by the instruction would have to discard it. Set aside in
favour of making exits rarer and cheaper; worth revisiting if exits still dominate somewhere.

### Dedicated C stack for native runs

GHC worker threads get 512 KB stacks, so the native call budget is about 250 KB, roughly 1600
frames of a small function; deeper recursion bounces through the interpreter every 1600
frames. Switching to a large malloc'd stack in `unison_jit_enter` would make that many times
rarer. Cheap to try; needs care with signal handlers and anything that inspects the stack.

### Oddity: text append performance is bimodal

`Text: append "hi" 10000 times` in `jit-benchmarks.md` gives either about 50 µs or 200 to
250 µs from one run to the next, on the optimized build with `--jit on`, from the codebase
and compiled alike ([2026-10-18](benchmarks/2026-10-18-compiled-programs.md)); the records
before that day only ever saw the slow mode. The fast mode would be the slack-chunk append
in place every time and the slow mode a copy per append, so something decides per run
whether the chunk keeps its slack (the CAS on the used length losing to a stale length,
or the first chunk's allocation), and finding it could make the row 4× faster every time.

### Linear re-entry code, and not counting preemption as a miss

Two things about re-entry functions, noticed 2026-10-20 when asking whether a
long-running function accumulates a re-entry point for every place it can be
interrupted.

**The code for a function's re-entry points is quadratic in its Lets.** A Let body's
combinator is generated as the whole suffix of the function from that Let on, inline,
so a function with n Lets in sequence has n suffixes, about n²/2 of its instructions.
The same holds for the deferred copies after slow-path call-outs (the continuation is
inlined in the parent on the fast path and again in each deferred function). The memo
per (section, depth, base) stops the nesting of bindings being exponential; it does not
remove the overlap of suffixes. In `eager` mode every Let body a Let refers to is
compiled with its group whether or not anything ever exits there (`groupUnits` with lazy
false), so the whole quadratic amount is paid up front. Certain call-outs are already
linear: the parent stops at the call-out and each segment appears once. The fix, for Let
bodies and deferred functions alike: generate a re-entry function as its own binding
followed by a call through the *next* Let's (or deferred site's) cell, a direct call when
that cell is compiled and otherwise the same exit the interpreter would take on an empty
cell, instead of inlining the rest. Only the root function needs the straight inline
path; the re-entry path is cold.

**A preemption counts as a miss at every Let on the stack.** A `Reenter` or `GrowStack`
exit in a callee unwinds through every native caller, each writing a frame record for the
Let it was in, and when `yield` later pops that `Push` frame with an empty cell `leap`
bumps the cell's count (`countReentry`). Timer ticks land wherever the stack happens to be,
so in `on` mode every Let of a hot function reaches the threshold eventually and gets its
body compiled, with no structural reason. The fix: `runNative` knows the status when it
builds the frames from the records (`pushRecord`), so build them with a "don't count" mark
for `Reenter` and `GrowStack`; `leap` still enters existing code through the cell, but
doesn't bump an empty one when the frame is marked. Then a body is compiled only for a Let
whose callee exits for a reason the code causes (not compiled, a handler, a certain
call-out, a slow path), which is bounded by the code. The cost is the interrupted caller's
suffix interpreted once per preemption, at most about 50 a second on a 20 ms timer.

## Rejected

### Unboxed values without the tag write

Every unboxed slot carries a type-tag closure, stored with every unboxed result; the thought
was to defer the store with a slot kind, as booleans are kept as an `i1` until they escape.
Measured 2026-10-07: the store goes to the slot's `alloca`, which `mem2reg` turns into a
register copy, so it already costs nothing. Of the roughly 3,700 tag stores surviving in the
suite's optimized IR, 3,277 were in cold exit and unwind blocks and the rest write a yielded
value or stack-passed arguments for a reader that can't know the kind. Loops carry no tag
store (the self tail call goes through the allocas), and workers pass the tag in a register.

### One write-back block per function

After exits and unwinds that write the same thing came to share a block
([2026-10-09 write-back](benchmarks/2026-10-09-write-back.md)), the next step tried was one
block per function, with the depth as a phi and the union of the live sets written. It took
the largest module from 193 blocks to 60 and the suite's IR from 3.8 to 3.0 MB, but the
compile time only from 1.06 to 1.04 s: the union is nearly every slot, and after `mem2reg`
the block's phis cost the backend the same copies on the incoming edges as the stores they
replaced. And `Cons.map` ran 25% slower in every run, the hot loop paying for values kept
live into the cold edges. Reverted the same day; the exit call
([2026-10-09 exit call](benchmarks/2026-10-09-exit-call.md)) is what replaced the blocks.

### `llvm.experimental.deoptimize` and stack maps for exits

The intrinsic records where the live operands are, in a table in the object file, instead of
storing them; a runtime routine reads the table. The plan for it stopped at the first
hand-written module: LLVM lowers the call as one that never returns (a trap follows it), so
the runtime routine must pop the enclosing frame and restore its callee-saved registers
itself, and the table says the frame's size but not where those registers are. That is
assembly per platform. `gc.statepoint`, the intrinsic underneath, returns normally, but its
site compiled to the same machine code as the varargs call that was done instead, plus a
table to parse. And the stores the assembly would have removed are not where the backend's
time goes: the exit call took them out of the IR and the backend's time stayed,
[2026-10-10 backend profile](benchmarks/2026-10-10-backend-profile.md).

### Fewer slot stores at call sites

The other half of the write-back idea was the argument stores of calls through the stack.
Counted 2026-10-09: about 1% of the largest module's lines (338 of 26,650). Nothing there.

### A cheaper optimizer or backend across the board

`default<O1>`, no passes, and the backend at O0 to O2 were measured against the default
([2026-10-09 O0](benchmarks/2026-10-09-o0.md)): the passes and the backend are each about
half of a module's compile time, and each is worth 5× or more at run time on the loop
benchmarks, so neither can be dropped. What was kept is `Config.defaultPasses`, `O2`'s shape
without the passes our code can't use, the same code on the suite at 60% of the pass time
([2026-10-09 pipeline](benchmarks/2026-10-09-pipeline.md)); every lighter pipeline on the way
lost a row, and the pass responsible was found by diffing the optimized IR. The vectorizers
switched off through the pass builder options made no measurable difference, so that knob
was not kept.

GlobalISel (LLVM's faster instruction selector) and the `basic` register allocator each save
7–10% of code generation and cost the hot loops 4–8% (`fib`, `Cons.map`, lookups, Collatz),
[2026-10-10 backend options](benchmarks/2026-10-10-backend-options.md). Both remain a flag
away through `UNISON_JIT_LLVM_OPTS` for anyone who wants that trade on purpose. The
remaining backend time is instruction selection, register allocation and the machine passes
in proportion to the code, no fixed cost and no single pass to target
([2026-10-10 backend profile](benchmarks/2026-10-10-backend-profile.md)).

### Keeping LLVM state across modules

The fixed cost per module is about 2 ms (0.6 ms to build the pass pipeline, the rest in the
code generator's setup and the link), 10% of the suite's compile time over 69 modules. The C
API builds the pipeline anew in every `LLVMRunPasses`; keeping a pass manager across modules
needs the C++ API, which loading LLVM at run time rules out. Measured 2026-10-09 with the C
harness ([compile time](benchmarks/2026-10-09-compile-time.md)).

### Two-stage batching with a clock, and warm sets

Two proposals for better batches (2026-10-04 and 2026-10-08): hold a hot function in a
pending batch with its neighbours until the batch reaches size B or a clock runs out; or mark
definitions warm at N/2 calls, propagate warmth along the call graph, and compile every warm
definition when any reaches N. Not pursued because the goals, callers in the batch and no
clock, are what the weighted batch rule does ([design](design.md#what-gets-compiled-and-when),
[2026-10-08](benchmarks/2026-10-08-weighted-batches.md)): the call graph is static and the
counts accumulate in the cells, so at the moment a definition reaches N a walk from it finds
exactly the set the warm marking would have built, without the state. The clock was the
problem with the first form: the wait is paid by a hot loop running interpreted, and `on`
matching `eager` on the suite was at risk. The one thing warmth could add is dynamic edges
(closures, handlers), which the static graph misses and which cost a write per call to
record. The holding area for fresh hot functions survives as the tiered idea above.

### Array digits in the Deque

Tried while the finger tree was built: array digits below the top level (pushes and pops
about 1 ns slower, append only 15% faster; half of append's time was then in
`copySmallArray#`, `memmove` and `newSmallArray#`, calls into the runtime even for three
elements), and a single array-only node constructor in place of the inline node of eight
leaves (pushes 7% slower, pops 20 to 45%, `sum` and `toList` 60 to 70%),
[2026-10-02](benchmarks/2026-10-02-deque-vs-data-sequence.md). The structure the Deque
replaced (worst-case O(1) pushes and pops, twice the code, slower on every operation) was
deleted; it is in the branch's history (commits of 2026-10-02).
