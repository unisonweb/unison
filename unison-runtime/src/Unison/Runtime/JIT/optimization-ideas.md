# JIT: optimization ideas

Things noticed along the way that aren't in any milestone plan yet. Each entry says what was
observed, what the change would be, and roughly what it might buy. When one is picked up it
moves into a milestone plan; when one is measured and found not worth it, say so here rather
than deleting it.

## Representation

- **Flat `GDataG`.** A constructor with three or more fields holds a `Seg`, which is a tuple of
  a `ByteArray` and an `Array`. A tuple's fields can't be unpacked, so the closure points at two
  lifted boxes, each pointing at the real array. Reading one field is three dependent loads
  (box, array, element); building the constructor allocates five objects and 15 + 2n words for n
  fields, where a flat closure would be one object of 3 + 2n words (info, reference, tag, then
  the fields). The tree benchmark's `Node` has four fields, so this is on its hot path. This is
  an interpreter change (`GClosure`, `dataBranch`, `buildData`, serialization of values), not a
  JIT one, so it needs its own plan. Noted 2026-09-30 during M3.

- **Unboxed values without the tag write: measured, nothing to gain (2026-10-07).** Every
  unboxed slot carries a type-tag closure in `bstk`, and the generator stores the tag pointer
  with every unboxed result. The thought was to defer that store with a slot kind, as booleans
  are kept as an `i1` until they escape. But the store goes to the slot's `alloca`, which
  `mem2reg` turns into a register copy, so it already costs nothing. In the optimized IR of the
  whole benchmark suite, 3,277 of the roughly 3,700 surviving tag stores are in `exit` and
  `unwind` blocks (cold), and the rest write a `yield`ed value or stack-passed arguments for a
  reader (the interpreter, or a function with the uniform signature) that can't know the kind.
  The self tail call goes through the allocas and a branch to the head, so loops carry no tag
  store, and workers pass the tag in a register. What remains is the non-worker calling
  convention itself; see the worker entries above.

## Allocation

- **Bump allocation inline.** Done 2026-10-03 (see
  [the measurements](benchmarks/2026-10-03-inline-bump-allocation.md)): native code and the C helpers bump a copy of `rCurrentAlloc`'s free pointer
  against the block's end and call `allocate` only on a miss. Allocation-heavy `jitSuite`
  rows 1.1× to 1.5× faster. GHC's own `Hp` couldn't be used: it is a callee-saved machine
  register during the unsafe call, not a word in the register table. The budget was folded
  into `hp_lim` on 2026-10-04 (the limit is the nearer of the block's end and the budget's
  end; the poll tests `hp` against the latter), for a further 1 to 6% on the rows that
  allocate most. What is left here is "One allocation for several `Pack`s" below, now worth
  less: with no call per allocation, batching saves only the compare and branch.

- **One allocation for several `Pack`s.** The design's original plan was one `allocate` per
  straight-line run. A single `Pack` needs no care: its fields are already evaluated, so nothing
  can exit between the allocation and the last field store. The only thing to respect when
  batching is that a run may contain an instruction that exits (`DIVN` on zero, a `ForeignCall`,
  an unsupported primitive) between two `Pack`s, and the objects after such an exit would be
  left unwritten in the nursery, which the debug RTS's heap walker would object to. So the batch
  boundary is "the next instruction that can exit", not "the end of the run": consecutive
  `Pack`s and the arithmetic between them share one call. Simple, and combines with the
  previous item.

## Compilation policy

- **`on` mode has two timing regimes on the suite.** The same build, run twice, gives either
  `Decode Nat` 157 ns, `Map.lookup` 194 ns, `List.foldLeft` 1.28 ms or 180 ns, 212 ns,
  1.20 ms: the benchmarks that go through the harness loop move one way and the list ones
  the other, by 7 to 15%. Exit and entry counts are the same in both. What gets compiled
  depends on timing (batches, and verdicts reached through a cycle), so the likely cause is
  a function that is compiled in one run and not the other. Both regimes are within the
  criterion; a deterministic choice would be better than either. Noted 2026-10-01, still
  open at the end of M7.

- **Which combinator's count means hot.** Any combinator of a group reaching the threshold
  compiles the group, including a local loop inside a function called once; a batch member
  is judged by its entry combinator only. Neither rule has been argued for against the
  other; noted as open in M5.

## Compile time

- **Where a module's compile time goes** (measured 2026-10-09 on the suite's largest module,
  `unison_839`: 35 definitions, 72 functions, 3.8 MB and 96k lines of IR, 1.2 s in the
  runtime; re-run offline with Homebrew's `opt`/`llc` 23 on the dumped text).
  Parsing the text: 0.24 s. `default<O2>`: 0.21 s (O1: 0.16 s). Backend at the default
  level: 0.94 s, of which the pre-RA machine scheduler is 44% and the post-RA scheduler
  29%; instruction selection is 12%, register allocation 4%. With both schedulers off
  (`-enable-misched=false -enable-post-misched=false`) the backend takes 0.36 s; the backend
  at O0 takes 0.10 s but with the fast register allocator. Two functions are 85% of the
  module: `jitSuite`'s own body (185 lines of MCode, frame size 128, 24 `printTime` rows)
  is 55k lines of IR, and the harness's row function 27k; the suite body runs once and
  joined the batch through its 23 call sites to a hot callee. The instruction mix is 33k
  loads, 20k GEPs, 18k stores and 17k adds against 600 branches and 277 calls: nearly all
  of it is slot traffic to and from the real stack around call sites and exits, in a few
  very long basic blocks, which is what makes the schedulers (superlinear in block length)
  the dominant cost. Ranked ideas, with the risk that each slows the generated code
  (1 and 2 done the same day: the suite's compile time halved, from 3.5 s to 1.75 s, and
  the timings didn't move, [2026-10-09](benchmarks/2026-10-09-compile-time.md)):
  1. Done: the machine schedulers are switched off through `LLVMParseCommandLineOptions`
     after LLVM is loaded (`UNISON_JIT_SCHED=1` keeps them). A third of what remained.
  2. Done: a candidate's estimated calls are divided by its size (`groupSize`, frame size
     times call sites, in units of `UNISON_JIT_SIZE_UNIT`) before the gate; the two giants,
     called once and ten times, stay interpreted, and a third of the suite's IR with them.
  3. Done the same day for exits and unwinds: shared write-back blocks, live slots only,
     [2026-10-09 write-back](benchmarks/2026-10-09-write-back.md), IR 6.9 → 3.8 MB and
     1.75 → 1.26 s. The argument stores of calls through the stack were the other half of
     this idea, but they are about 1% of the largest module's lines (338 of 26,650), so there
     is nothing there. The write-back blocks are still 75% of that module after the live
     sets: 193 blocks for 204 sites (the sites differ in depth), 15 live slots each, 6 lines
     a slot. Tried the same day and reverted: one block per function for the exits, with
     the depth as a phi and the union of the live sets written (every slot initialized at
     entry, so that a slot above a shallow exit's depth holds a defined pointer; harmless
     to write, since the stack above the frame is free). It took the largest module from
     193 to 60 blocks and the suite's IR from 3.8 to 3.0 MB, but the compile time only
     from 1.06 to 1.04 s: the union is nearly every slot, and after `mem2reg` the shared
     block's phis cost the backend the same copies on the incoming edges as the stores
     they replaced. And `Cons.map` ran 25% slower in every run (13 → 16 µs), the hot loop
     paying for the extra values kept live into the cold edges. Done 2026-10-10 instead:
     one call per site to a C routine with the live slots as variadic arguments and a
     constant descriptor, [2026-10-09 exit call](benchmarks/2026-10-09-exit-call.md):
     IR 3.8 → 2.9 MB, compile time 1.03–1.14 → 0.88–0.93 s (parse and optimize 0.39–0.49 → 0.27–0.30 s; the backend unchanged), lookups 10% and Collatz 4% faster, murmur 3% slower since its worker is no longer inlined (the inliner charges for the call at each site; `UNISON_JIT_INLINE_THRESHOLD=300` recovers it, left at the default for now). Not `llvm.experimental.deoptimize`: its runtime routine has to pop
     the frame itself (assembly), see the file. Originally: less slot traffic per call site and exit: shared write-back blocks (below under Code
     size), saving only the slots live across the call, or a helper call for a frame spill
     (medium to large gain across all three phases, and smaller code; medium risk at hot
     call sites, none on exits).
     Where the backend's time goes, profiled 2026-10-10 after this:
     [backend profile](benchmarks/2026-10-10-backend-profile.md). Instruction selection,
     register allocation and the machine passes in proportion to the code, no fixed cost
     (linking under 1%), no single pass to target. The levers left for compile time are
     upstream: fewer lines reaching the backend, fewer modules compiled.
  4. Build the module through the C API or as bitcode instead of text (saves the parse;
     no risk; large change to the generator). After 1 to 3 the suite's 1.38 s splits into
     generating the IR 0.10 s, LLVM's parse and `O2` 0.68 s (of which the parse is about a
     tenth by the offline numbers), and LLVM's code generation 0.59 s, measured 2026-10-09
     with the phase split in the stats line. A C harness driving `jit_llvm.c` in-process on
     dumped modules (same day) shows the cost is nearly all proportional to the IR: the
     parse is 0.2 ms for 11 KB and 6 ms for 560 KB (about 1% overall), `O2` about 0.13 ms
     per KB and code generation plus linking about 0.11 ms per KB, with a fixed cost per
     module of about 2 ms (0.6 ms to build the `O2` pipeline, the rest in the code
     generator's setup and the link), which over 69 modules is 10%. The C API builds the
     pass pipeline anew in every `LLVMRunPasses`; keeping a pass manager across modules
     needs the C++ API, which the run-time loading rules out. A function-level pipeline
     (`mem2reg,sroa,early-cse,simplifycfg,instcombine,gvn,dse,simplifycfg`) costs a quarter
     of `O2` and makes code generation 10 to 15% cheaper, but has no inliner and no loop
     passes; with `cgscc(inline)` between two such function passes it would be the thing
     to measure against the suite, since the generated code's quality is what is at stake.
  5. Tiered: the trigger alone first in a small module, the batch later with direct calls
     (cuts the latency of the hot loop running interpreted, not the CPU; no risk).
  6. Several compile threads in LLJIT (`LLVMOrcLLJITBuilderSetNumCompileThreads`): wall
     clock only, and only across modules, when the time is in two or three big ones.
  7. `O1` or a hand-picked pipeline instead of `default<O2>`: 50 ms here, some risk; last.
     Measured 2026-10-09, [O0](benchmarks/2026-10-09-o0.md): the passes and the backend are
     each about half of a module's time, and each is worth 5× or more at run time on the
     loop benchmarks, so neither can be dropped; `UNISON_JIT_PASSES` and
     `UNISON_JIT_CODEGEN_LEVEL` exist for such measurements. Done the same day,
     [pipeline](benchmarks/2026-10-09-pipeline.md): `Config.defaultPasses` is `O2`'s shape
     without the passes our code can't use, the same code on the suite at 60% of the pass
     time, a fifth of the compile time. Lighter pipelines on the way each lost a row, and
     the pass responsible was found by diffing the optimized IR.

## Batching tweaks

- **The lag between a request and its batch** (measured and settled 2026-10-09,
  [benchmarks](benchmarks/2026-10-09-compile-thread-wake-up.md)). A request carries its
  time, and the log reports how long it waited. Idle compile thread: about 40 µs, on both
  builds. But a thread woken on a busy capability runs only at that capability's next
  scheduling point, and `forkIO` had put the compile thread on the capability of the thread
  that started the runtime; when that was the interpreter's, the first request waited up to
  1.2 ms, which is what made the batching benchmark need a 20,000-iteration warm-up. Now the
  compile thread is forked with `forkOn` onto another capability and a request yields after
  queuing, and the first request takes 36 µs every time. On the suite the distributions with
  and without are the same (median 36 µs against 37), since there the two threads mostly
  weren't sharing a capability anyway. What remains of the lag is the compile in progress: a
  request that arrives during one waits for it, 10 ms to 1 s, which is the first-install
  latency that matters and the argument for bounding a module's size.

- **Two-stage batching** (superseded 2026-10-08 by the weighted batch rule, below). The idea here is when a function becomes hot, add it to a pending compilation batch along with (breadth-first) traversal of up to D of its transitive dependencies. Then wait for the pending batch to reach size B (or perhaps up to 30s if pending batch stops growing). Compile those ~ B definitions together. The idea is this forms better batches which include related collections of callers and callees, better than compiling batches more eagerly. I also wonder if this approach can be used when lazily compiling re-entry points. If the re-entry functions are added to the pending batch, then there's an opportunity for multiple re-entry points to get compiled together.

  Reply (2026-10-04): agree, with two cautions and one addition. (1) The wait is paid by a hot
  loop running interpreted; `on` currently matches `eager` on the suite and a second delay on
  top of the threshold puts that at risk. The clock should be on the order of a module compile
  (flush when the queue has been quiet for a couple of compile times, bounded at a few hundred
  ms, and let a fast-climbing counter force a flush), not 30 s, which for a transcript means
  never. (2) Walk callers, not only callees: M5 measured that callees are already compiled by
  the time a caller is hot (callees-only formed one multi-group batch in the whole transcript,
  callers took it to 16 of 55), and what a wait really buys is time for the callers to cross
  the threshold and join. Re-entry points are the clear win: each is a module of its own today
  and they outnumber functions (494 against 524 on the suite), they can't gain from direct
  calls anyway (their parent is in an older module), so coalescing them is pure fixed-cost
  saving with nothing lost. The addition: since install is one pointer write per cell, the seed
  could be compiled at once in a small module and the related set again later as one module
  with direct calls, replacing the pointers (tiered), which separates latency from batch
  quality if the wait turns out to hurt. First measurement to take: the fixed cost of a trivial
  module, and the module count `on` produces on the suite (193 in the last run).

- **Warm definitions, hot together.** (Paul, 2026-10-08; done the same day as the weighted
  batch rule: `formBatch` is Prim's algorithm over the call graph with edges weighted by
  estimated calls, gated at N/4, [design](design.md#what-gets-compiled-and-when). On the suite
  it halves total compile time (the breadth-first walk made one 6-definition module that took
  1.4 s; weighted compiles that definition alone in 10 ms) and a benchmark whose callee is
  first reached after its caller is hot runs 3× faster,
  [2026-10-08](benchmarks/2026-10-08-weighted-batches.md). Still open from the discussion: a
  penalty term in the weight for the callee's size, so that cutting an edge into a large
  callee counts for less (done 2026-10-09, see Compile time below: the 35-function module
  was the suite's own body joining on its 56 call sites); a bound on a module's size in IR
  rather than in definitions, with the trigger compiled alone first (tiered) so that it
  doesn't wait on the batch; and the lag below.) At N/2 calls a definition is warm;
  anything that calls a warm definition is warm, and anything a warm definition calls is warm
  once it has N/4 calls. When any warm definition reaches N, every warm definition is hot and
  they are compiled together: no clock, and the window from warm to hot is time for callers
  and callees to join.

  Reply (2026-10-08): the goals are the right ones, callers in the batch and no clock, and the
  callee gate at N/4 is worth trying by itself. But the warm state adds nothing: the call graph
  is static (the reverse index exists already) and the counts keep accumulating in the cells,
  so at the moment a definition reaches N, a walk from it over the static graph, taking callees
  with N/4 and callers by their count, finds exactly the set the warm marking would have
  built. The one thing warmth could add is dynamic edges (closures, handlers), which the static
  graph misses and which cost a write per call to record. Two changes to the rule: callers need
  a count too (unconditional propagation, applied transitively, warms the whole program up to
  `main` through any widely used helper; a caller called once with a hot loop is covered by the
  loop's own counter), and the batch should be the component reached from the trigger, capped
  at B, not one global warm set. So: `formBatch` with the callee gate at N/4, an explicit
  caller gate, and the walk made transitive in both directions. What it doesn't fix: the two
  timing regimes, which come from whether a neighbour is already compiled when the batch forms;
  private copies are the remedy there.

- **Recompile recently compiled callees with the callers that arrive later.** (Paul,
  2026-10-04.) Keep recently compiled definitions in the compile thread for a while (LRU or
  `SemispaceCache`); when a new caller enters the pending set and depends on one, include
  that callee in the caller's batch, so it is compiled again with its caller. Things get
  compiled as soon as they are hot, and a callee still ends up in one module with callers that
  show up soon after. Worry: code size, since a function then exists in several copies.

  Reply (2026-10-04): this is "Private copies of compiled callees" below with a recency window,
  and the two fit together as one plan. Two refinements. (1) Recency isn't the criterion:
  every definition's MCode stays in the code cache, so the compile thread can recompile
  anything at any time with nothing kept around; what matters when a caller is compiled is
  which of its callees are native already and small enough that inlining pays, which the call
  graph and the IR size recorded at the callee's compile answer without a clock. (2) The bloat
  depends on the kind of copy. A fresh public copy with the cell repointed leaves a whole body
  behind each time (the old one can't be freed: native frames may hold return addresses into
  it), so copies grow with the batches a popular callee joins. A *private* copy (internal
  linkage, called only directly from inside the module) is inlined at its call sites and then
  deleted by LLVM, so for the small callees that are the point there is no extra code at all;
  the original cell keeps the first module's code for everyone else. So: private copies, for
  the callees where a call is a real share of the work per call. (Paul: size is only a proxy
  for that.) `JIT.Estimate`'s path score is the better proxy: copy a callee whose estimated
  work per call is under k times a call's cost. Two tiers of gain (Paul, 2026-10-04): a copied
  callee is reached by a direct worker-to-worker call whatever else happens (arguments and
  results in registers, no round trip through the Unison stack, no cell load, `musttail` for
  tail calls), and LLVM may inline it on top. So the floor is a cheaper call, which justifies a
  generous k; a copy that is inlined everywhere is deleted, while one that is only called
  directly stays as a second body with its exits and frames registered again, so a size cap on
  the callee bounds that case (a large callee's call overhead is a small share of its work
  anyway). With that in place, waiting to form batches (two-stage
  above) buys less, since a late caller gets its callees inlined anyway; what batching still
  buys is the per-module fixed cost, which the re-entry-point coalescing covers.

- **Private copies of compiled callees, for inlining across definitions.** Done 2026-10-04
  (see [the measurements](benchmarks/2026-10-04-private-copies-and-reentry-batches.md)): callees whose estimated work
  per call is under `UNISON_JIT_COPY` and that don't loop or recurse are compiled again into
  the caller's module with internal linkage; in the test transcript every copy was inlined and
  deleted by O2. The re-entry-function batching from the discussion above is done too
  (`UNISON_JIT_REENTRY_WAIT`). Not done: the two-stage holding area for fresh hot functions
  (the latency argument above), and copies in `eager` mode (which compiles each group as it is
  loaded, before its callers exist). Left to measure: a program where a hot loop calls small
  library functions from other definitions, which is where this should show; the suite's
  steady-state rows did not move (every copy there was of something already cheap to call),
  and `List.map murmurHash` read 5 to 12% slower, which the next day's run showed to be
  that row's noise.

- **Compile a function's hot re-entry points together.** Each re-entry function asked for is a
  module of its own. Requests for the same function that are queued together could share a
  module, and then call each other directly. Noted 2026-09-30.

## Calls

- **Workers with register arguments.** Done in M6 (see the design, "Workers"). Left over:
  - Re-entry functions still pass everything through the stack. One that is hot (a loop
    that exits every iteration re-enters every iteration) could get a worker-like body.
  - Functions that return several values keep the uniform form.
  - The registers saved for exit paths: half of `fib`'s per-call cost. Every native
    function has many exits, and each needs the frame's values to write back; LLVM keeps
    values that are live across a call in callee-saved registers and saves and restores
    every one of them on every call, whether or not an exit is ever taken (`fib`'s worker
    saved ten, six of them only for its exit paths). Branch weights helped a lot, computing
    frame offsets at their use a little, and the fast entry sidesteps it for base cases;
    `-regalloc-csr-first-time-cost` changed nothing. Ideas: write only the slots the continuation can read (needs liveness
    for frame slots, and a look at what captured continuations do with dead slots); or
    keep type tags out of registers by passing unboxed values without them (see
    "Unboxed values without the tag write").
  - The type-tag closures are loaded through the pool at entry (two loads); they could be
    fields of `Ctx`.

- **Direct calls within a module.** Done in M5: functions compiled together call each other by
  symbol, and LLVM can inline them.


- **Calls through dynamic-scope references (`App (Dyn i)`).** Calling an ability handler
  (`Counter.next` inside `handle`) resolves the function through the handler environment,
  which lives in Haskell (`HEnv`), not on the Unison stack, so native code exits. Passing the
  current `denv` into `Ctx` (or a pointer to it) would let native code resolve it with a map
  lookup in C, or a call-out could return the value. Seen in the test transcript's ability
  loops: one exit per handler call. Noted 2026-09-30 during M4.

- **Tiny native functions called from the interpreter.** Done in M6 step 8: the verdict
  "too small to enter" keeps the interpreter interpreting them. A leaner entry for leaf
  functions would be the other way.

- **Native ability requests.** A request (`App (Dyn i)`) is an exit today, and so is
  everything a handler starts with (`RMatch`, `Capture`, `InLocal`, `SetAff`, `Reset`),
  because the handler environment and `K` belong to the interpreter. The affine case (the
  handler runs and returns to the requester, no continuation captured) is the one that
  could be made native without continuation capture: it needs the dynamic environment
  readable from native code, `RMatch` as a match on the request's closure, and `InLocal`
  and `SetAff` as updates to an environment native code can see. Looked at in M6 step 6
  and left for a plan of its own; until then the static rule keeps request-heavy code
  interpreted. Noted 2026-10-01.

- **Build IR without `String`.** The generator builds each module as Haskell `String`s
  (megabytes of them), which is most of what the compile thread allocates. That
  allocation is shared with the running program: more GCs, and major ones sooner (M6:
  a one-shot 20 ms benchmark absorbing a 26 ms collection during warm-up). A builder over
  byte arrays, or LLVM's C API for building IR directly, would cut it. Noted 2026-10-01.

- **Hoist the self cell load.** A function's own cell is loaded before every self non-tail
  call; loading it once at entry is enough (the cell can't change under a running function).

- **Move the pending-arguments check to entry.** Done for workers in M6: the wrapper
  checks, and workers have no `ap`. Functions with the uniform signature still compare
  before every return.

## Code size

- **Re-entry functions on demand.** Done in M5 for the `on` mode:
  a function's first compile is linear in its size, and a re-entry function is generated when
  a counter says it is used. Still generated with the function: the continuation of a call-out
  with no fast path, which could be counted too.

- **Shared frame write-back for exits.** Done 2026-10-09, in two parts
  ([internals](internals.md#the-generator), "Write-back"): exits and unwinds that write the
  same thing share one block with the status as a phi, and only the slots the interpreter
  will read are written (`liveAt`, a walk of the resumed section). Sharing alone took the
  suite's IR from 6.9 to 5.7 MB but compile time by only 5%: the re-entry functions of a
  chain of `Let`s exit at a different depth each, so little was shared. The live sets took
  it to 3.8 MB and the compile time to 1.26 s, with `fib` 15% faster on top (fewer values
  kept across calls for the exit paths), [2026-10-09 write-back](benchmarks/2026-10-09-write-back.md).
  (Originally: every exit block stores every slot back to the Unison stack,
  so a function with many exits and a large frame is mostly exit code; `Duration.toText`,
  one function, 37 exits, 398 KB of IR. Noted 2026-09-30.) Still open: the live sets of the
  rope and decode code are 15 of 25 slots, since deep `VArgN` reads keep early slots alive,
  so the remaining write-back is real. Since 2026-10-10 what is left per slot is one
  argument of the call to `unison_jit_exit_frame` (a spill by the backend); the IR carries
  nothing per slot but the operand. Still open, if ever worth it: `preserve_most` leaves the
  spills in place; passing the values' *locations* instead (a stack map) would remove them,
  but LLVM's only portable way of doing that is `llvm.experimental.deoptimize`, whose runtime
  routine has to pop the frame and restore callee-saved registers itself, assembly per
  platform. Not worth it: the backend's share of the compile time didn't move when the
  stores became arguments, so the spills are not where its time goes.

- **Fallback: tail-call the body function at the end of an inline binding.** If lazy generation
  turns out not to be enough, an inline binding could end with a `musttail` call to the body
  function instead of having the body inlined after it, so every body exists once. This costs
  the fast path its registers: the binding's results and every live slot travel through the
  Unison stack at each `let`, which is what the interpreter does, and LLVM can no longer
  optimize across the boundary. Noted 2026-09-30 during M4; demoted to a fallback when M5 was
  planned. M5 measured the case it is for: with a threshold of 1, so that every re-entry point
  met is generated, the benchmark transcript spends 8.5 s compiling against 0.8 s normally.

## Builtins

- **Native `Text` operations.** (Size, `++`, take, drop and equality were done in M6 step
  5b; what is left is in "More of `Text` natively" below. The rest of this entry is the
  original note, and the rope it describes was replaced by a finger tree on 2026-10-02:
  see "The rope itself" below.) Text-heavy code bounces through the trampoline on every
  `Text.++`, `Text.size`, `Int.toText` and so on, and each bounce also unwinds every native
  caller above it, which makes their `let` re-entry points hot (`Duration.toText` has about 40
  call-out continuations, nearly all for `Int.toText` and `++`). A Unison `Text` is a
  `Rope Chunk` with all fields strict: `Empty | One !Chunk | Two !Int !Rope !Rope`
  (`lib/unison-util-rope`), and `Chunk !Int !T.Text` unpacked (a character count, then the
  UTF-8 byte array, offset and length; `parser-typechecker/src/Unison/Util/Text.hs`). `Text`
  itself is a newtype. So native code or a C helper can pattern match on it directly, with
  layouts from the startup probe as for the data closures, and strict fields mean no thunks
  or indirections to worry about. In rough order of value and ease:
  - `Text.size`: the count is in the root node. A few loads, no allocation.
  - `Text.++`: the `Empty` cases return the other side; the balanced case allocates one `Two`
    node (four words). The unbalanced cases (`appendL`/`appendR`) and the merge of two small
    chunks (a new byte array and two `memcpy`s) can be a C helper called directly from native
    code, or stay on the call-out as the slow path at first.
  - `Text.==` and comparison: the single-chunk case is a length compare and `memcmp`;
    anything else calls out.
  - `Nat.toText` / `Int.toText`: a C helper that formats into a fresh byte array and wraps it
    in `One (Chunk n text)`.
  - `take`, `drop`, `uncons` and friends index by character over UTF-8, so they need a scan;
    later, probably as C helpers.

  The pattern is M4's for `Ref` and arrays: a native fast path with the existing call-out as
  the slow path, so the interpreter's implementation stays the source of truth for the hard
  cases. Things to check: the result should be the structure the Haskell code would build
  (the rope's balancing and the 512-character chunk threshold affect the cost of later
  operations, though not their results); allocation goes through the budgeted allocator; the
  debug runtime's heap checks cover the new objects. Prioritize by the call-out counts from
  `UNISON_JIT_STATS` on real programs. `Bytes` is the same `Rope` over byte chunks
  (`lib/unison-util-bytes`) and would follow the same way. Each builtin done this way also
  removes a call-out continuation and cools the re-entry points of its callers. Noted
  2026-09-30 after M4.

- **No thunks on the boxed stack.** Paul's preferred fix for the untagged-pointer crash
  (2026-10-03; see the internals, "Untagged pointers"):
  make the interpreter guarantee that a boxed stack slot only ever holds an evaluated, tagged
  pointer, instead of (or as well as) native code checking the tag on every store into a
  strict field. The hazard funnels through `bpoke`, `bpokeOff` and `poke` in `Stack.hs`. The
  one source found is a top-level constant poked as itself: `writeBack stk Nothing = bpoke
  stk noneClo`. The bang on `bpoke`'s argument can't help there, because what the slot
  receives is the address of the static closure (`THUNK_STATIC`, then `IND_STATIC` once the
  CAF has been forced), and that address is untagged forever, whether or not the CAF has
  been evaluated. So the untagged pointer appears on *every* store of such a constant, not
  just the first; `requireTagged` is cheap because native code rarely stores one, not
  because it happens once. (An earlier write-up also blamed a lazily built `Some`,
  `someClo (encodeVal v)`; that was an inference, not observed, and it is wrong: the bang
  forces the application to WHNF and the slot gets the evaluated, tagged `Data1`, or, if
  GHC inlines `someClo`, a fresh constructor, which is always tagged, with its strict `Val`
  field evaluated too. The log agrees: `Bytes.at 0 0xsdeadbeef`, a `Some`, never crashed.)
  The fix is correspondingly narrow: `evaluate` (`seq#`) in `bpoke` and friends, storing
  its *result* rather than the pointer handed in (for a CAF that is the indirectee, a tagged
  constructor), or constants that are genuinely static, which would need `Ty.optionalRef`
  and the other references they mention to be literals rather than computed `Reference`s.
  Either way, grep the `bpoke`, `bpokeOff` and `poke` call sites for other top-level
  `Closure` constants stored directly; `noneClo` is only the one that was hit. Cost of the
  `evaluate` route: a tag test and an untaken branch per boxed store, to be measured on the
  suite; it may even help the interpreter, since nothing downstream enters a thunk on read.
  What it buys: a real invariant that is simpler than "every native write checks", covers
  places not instrumented, and makes the read-side guard in `genDMatchClosure` stop costing
  a resume for a `None` from a call-out. Plan: temporarily disable `requireTagged`, confirm
  the old reproducers (`> Bytes.at 0 0xs`, `> Text.uncons "abc"`) crash again, land the
  `bpoke` change, confirm they pass, re-enable the checks as a cheap safety net (a violation
  is silent heap corruption found two GCs later), and measure both the interpreter and
  `jitSuite`.
- **`Bytes` operations.** Done 2026-10-02 and 2026-10-03: every Bytes primitive and the
  pure foreign functions are native (see the internals, "Data representations").
  Left as call-outs: the compression functions (`zlib`, `gzip`, `zstd`: library-bound; a C
  port would mean linking those libraries into the runtime's C side), and the exceptions and
  error messages, which the interpreter raises on the slow path.
- **Bytes literals built directly, through a separate MCode rewrite pass.** (Paul,
  2026-10-03.) The parser turns `0xsdeadbeef` into `Bytes.fromList [222, 173, 190, 239]`
  (`TermParser.hs` and `Term.hs` do this, and the printer undoes it), so a bytes literal
  compiles to a list literal of n boxed Nats followed by a `Bytes.fromList` call, both
  native since 2026-10-03 but still n allocations, a finger tree and a conversion at every
  evaluation. The desired code is the direct construction: a `Bytes` constant that the pool
  holds like a text constant, so the literal costs one pool load. There is no literal form
  for it yet: `ANF.Lit` and `MLit` have cases for numbers, chars, text and links but not
  bytes (the `BLit (Bytes b)` constructor belongs to `ANF.Value`, the serialized value
  form, not to code), so the rewrite needs either a new `Lit`/`MLit` case (which touches
  the code serialization and its version) or a JIT-side pseudo-instruction that only the
  native path understands, with the original list-and-call kept for the interpreter.
  Recognize the pattern (a `Seq` literal whose
  elements are all Nat literals in 0..255, consumed only by `Bytes.fromList`) and replace
  it. Recognizing it at the MCode level may be awkward: by then the list is a `Pack`/list
  literal over stack slots filled by earlier `Lit` instructions, and removing those
  bindings means renumbering the variable indices of everything after them in the
  section. Doing it on ANF, before `emitSection` assigns indices, avoids the renumbering,
  and is where the information is still structural. Either way, Paul wants these rewrites
  kept modular and separate from codegen: a pass (or a small framework of rewrite rules)
  over ANF or MCode that the compiler runs before emitting, so that further
  optimizations and rewrite rules can accumulate there without each one being threaded
  through `Codegen.hs`. Candidates for the same pass later: constant folding of arithmetic
  on literals, `Text` appends of literals, `List` literals of constants as pool values.
  With a real `MLit` case the interpreter benefits too. `Decompile.hs` prints a bytes
  value as `Bytes.fromList [...]` and the printer shows that as `0xs...`, so printing is
  unaffected either way.
- **More of `Text` natively.** Done 2026-10-03: every Text primitive and the pure foreign
  functions are native; see the internals, "Data representations". Left as call-outs: the Text patterns
  (`Text.patterns.*` build pattern values that `Pattern.run`/`isMatch` interpret in Haskell;
  Paul, 2026-10-03: skip for now), `Link.toText` (hashing), `toUppercase`/`toLowercase` of
  non-ASCII text (Unicode case mapping tables), and the forms of `Int.fromText` and friends
  that the Haskell lexer reads beyond sign and digits (hex, spaces, "NaN"). The ASCII-only
  case mapping could be widened with a table for the common scripts if a program needs it.
- **The rope itself** (`Unison.Util.Rope`, a finger tree of chunks since 2026-10-02;
  numbers in [the rope benchmark](benchmarks/2026-10-02-rope-vs-old-rope.md)):
  - *`Json.toText` is 20% slower than on the old rope and the cause is not found* ([what was tried](benchmarks/2026-10-02-text-and-bytes-helpers.md)). Worth one more look at the interpreter's `catt`
    path: the same `"[" ++ x ++ "]"` is three times faster through the C helper.
  - *Appending two large texts is slower than it was.* The size-balanced tree it replaced
    made one node when the two sides were within a factor of two of each other (20 to 30
    ns); the finger tree packs the inner digits into nodes at every level (150 to 290 ns
    when both sides have a middle, which takes more than about 20 chunks each). It is the
    Deque's append, so whatever makes that cheaper (see "The list structure itself") helps
    here too. A text built by appending short pieces doesn't go through this path.
  - *A chunk with room to grow.* Appending a short piece to a text copies the last chunk
    into a new array each time (up to the threshold's worth of characters). A last chunk
    with spare capacity that the next append writes into, as a string builder does, would
    make it one small copy. A persistent structure needs to know that nobody else has
    appended to the same array (an owner's fill mark, checked and advanced atomically), so
    this is a design of its own.
  - *Characters of one byte.* A chunk whose character count equals its byte length is
    all ASCII, and indexing, taking and dropping in it need no scan. `Data.Text`'s
    operations don't know that; the chunk instances in `Unison.Util.Text` and the C
    helpers (`utf8_prefix`) could check it. Chunks loaded from a literal are up to 512
    characters, and the scan is most of `Text.at`, `take` and `drop` on them.
  - `take` and `drop` build the part of the rope before the cut and then `snoc` the cut
    chunk's piece, which allocates the top level twice. Building it once would take the
    10 to 20% they trail the old rope by on texts of short chunks.
  - Equality and comparison in Haskell go through lazy lists of chunks (as before), and in
    C find each next chunk by its position, which costs a walk down per chunk. A cursor
    that remembers where it is would make both a single pass.
  - The threshold (64) was chosen from 16, 32, 64 and 128 on the rope benchmark: 64 and
    128 halve the cost of walking and comparing against 32 and cost nothing when building;
    128 makes appending pieces of 40 characters and indexing short-chunk texts slower.
  - Any change here has to be made in the C helpers too, which the startup checks and
    `UNISON_JIT_STRESS=texts=N` enforce (development.md).
- **What is left of lists.** Every list primitive is native since 2026-10-02 (the C
  helpers are ports of `Unison.Util.Deque`). What still leaves native code or costs more
  than it should:
  - The list operations that are foreign functions rather than primitives (`List.sort`,
    conversions from `Text` and `Bytes`): call-outs like any other foreign function.
  - Every operation is a call into C, including the common push and pop, which are a
    dozen instructions. Generating those two cases inline (the digit has room; the digit
    keeps an item) and calling C for the rest would remove the call and let LLVM see the
    allocation.
  - A list literal is one call per element plus one to wrap the result. A helper that
    takes the elements from the stack and builds the tree in one pass (what `fromListN`
    does) would be better for long literals.
  - `SPLL`/`SPLR` do a take and a drop, two walks to the same place; a single split would
    share the walk.
- **Partial applications of function values.** M6 step 7 builds a partial application
  natively when the function is known at code generation. A function value applied to too
  few arguments (`g = f x` where `f` is itself a closure) still resumes in the interpreter:
  whether the call is under-saturated is only known at run time, so the closure-call code
  needs a second continuation that joins the body with the new closure instead of a call's
  result. The helper already handles closures with arguments captured. Also: more than
  four arguments at once.
- **The list structure itself** (`Unison.Util.Deque`, a strict finger tree, the runtime's
  list since 2026-10-02; numbers in [the Deque benchmark](benchmarks/2026-10-02-deque-vs-data-sequence.md)
  and [the suite runs](benchmarks/2026-10-02-native-lists-in-the-suite.md), where it is called
  Deque2, its name while the structure it replaced still existed):
  - *JSON parsing is slower than it was on `Data.Sequence`, and the list is not why.* With
    the JIT off: 7.2 µs per document on `Data.Sequence`, 10.2 µs on the old Deque, 10.1 µs
    on Deque2, whose pushes are faster than both; complex parsing 10.7, 18.1, 18.1 µs. So
    something else changed with the first swap. Candidates: the conversion at the
    `ANF.Value`/`Term.List` boundary, `fromList`, or the parser's use of `Sq.empty` and
    `|>` no longer fusing with something. A profile of that one benchmark is the place to
    start. `List.range` per element (42 ns to 102 ns, a very large list built in one go)
    is in the same position.
  - The rope's append chooses which side's digit to copy when both sides have no middle
    (the shorter one); `Deque.append` always extends the left prefix, so `acc ++ [x, y]`
    on a list of up to twenty elements copies `acc`'s cells. The same choice is a few
    lines there and in `lv_append`.
  - *Where it trails `Data.Sequence`:* `append` of two large pieces (1.5 to 1.8 times),
    `drop` (1.1 to 1.3), `fromList` of a long list (3 times; it is a fold of `snoc`, and
    the bulk `fromListN` measured no faster for a reason not yet found). Append and drop
    spend their time reading digit lists cell by cell on data that is not in the cache.
    Digits that are one object, without the array calls, are the thing to try: an array
    with an offset so pops don't copy, filled by single writes.
  - *What was tried and lost while it was built:* array digits below the top level (pushes
    and pops about 1 ns slower, append only 15% faster; half of append's time was then
    inside `copySmallArray#`, `memmove` and `newSmallArray#`, which are calls into the
    runtime even for three elements), and a single array-only node constructor in place
    of the inline node of eight leaves (pushes 7% slower, pops 20 to 45%, `sum` and
    `toList` 60 to 70%).
  - Any change here has to be made in the C helpers too, which the startup checks and
    `UNISON_JIT_STRESS=lists=N` enforce (development.md).

  The structure it replaced (worst-case O(1) pushes and pops, about twice the code, slower
  on every operation measured) was deleted on 2026-10-02; it is in the branch's history,
  in the M5a commit.
  `Unison.Util.Skews` (two skew binary lists back to back, also Paul's) is in the same
  package as a possible alternative: it compiles, nothing uses it, and it hasn't been
  measured against either structure.

## Allocation budget

- **The budget with a small nursery.** The allocation budget relies on the interpreter's
  next heap check to trigger the GC after a budget exit. That worked in the memory test
  (bounded residency, 400 budget exits over 20 M cells) and under `alloc=64`, but it hasn't
  been tried with a small nursery (`+RTS -A256k`) or on a capability shared with other busy
  Haskell threads. Open since M3 (2026-09-30).

## Release

Taken out of M6 on 2026-10-01 so that milestone could be about performance on real programs;
for a later milestone.

- **Compiled programs** (`run.compiled`, the standalone path in `Interface.hs` that
  restores a code cache from a file) build their combinators without native code cells, so
  the JIT does nothing there. Noticed 2026-10-01, not checked further.
- **Static linking of LLVM** (D4 said dynamic first, static before release).
- **Linux x86-64**, then the remaining Unix platforms (D6). Skipped so far: every spike and
  test has run on macOS arm64 only.
- **Documentation** for users: the modes, the environment variables, what to expect.
- **Whether the JIT is on by default**, and whether the `jit` package flag stays.
- **Native versions of more builtins** as call-out statistics from real programs show them
  to matter. [builtins.md](builtins.md) is the checklist of what is native today, with
  the op or foreign function each missing one waits on. Int, Nat and Float are complete
  (2026-10-04), and so are Arrays, Refs and tickets (2026-10-05). What is left is effects,
  reflection, hashing, the FFI and the big numbers, which are wrappers around Haskell libraries;
  `Universal.murmurHashUntyped` is native (2026-10-04, except for maps: the Haskell hash
  rebuilds a map with `fromDistinctAscList` and hashes that tree, which a C port would have to
  reproduce); `Universal.murmurHash` would need the value serialization ported. Prioritize by call-out
  statistics from real programs (see "Builtins").

## Stack and frames

- **Call-outs without unwinding: native code on its own C stack.** Today an exit in a callee
  unwinds every native caller, and each comes back through a re-entry point. If native code
  ran on a C stack of its own, a call-out could switch back to the Haskell thread's stack,
  let the interpreter run the one instruction, and switch back in, with all the native frames
  still there. The obstacle is the GC: it can run during that instruction and move objects,
  so every parked native frame would need its live values on the Unison stack before any
  call and would have to reload them afterwards (a flag set by the trampoline could make the
  reload conditional). There would also be a parked stack per Unison thread and per nested
  evaluation, and exceptions thrown by the instruction would have to discard it. Considered
  when M6 was planned and set aside in favour of making exits rarer and cheaper; worth
  revisiting if exits still dominate after M6. Noted 2026-10-01.

- **Dedicated C stack for native runs** (D13). GHC worker threads get 512 KB stacks, so the
  native call budget is about 250 KB, roughly 1600 frames of a small function; deeper recursion
  bounces through the interpreter every 1600 frames. Switching to a large malloc'd stack in
  `unison_jit_enter` would make that many times rarer. Cheap to try; needs care with signal
  handlers and with anything that inspects the stack.

- **The Unison stack as a linked list of chunks.** `ustk` and `bstk` are single arrays, so
  growing the stack is a reallocation and a copy of everything below `sp`: `ensure` grows by
  a fixed 1280 slots, and the trampoline's `ensureGenerously` by at least the current size,
  since native code pays for each growth with an unwind of every native frame
  ([internals](internals.md#the-trampoline)). GHC's own threads haven't had a copying stack
  since 7.2: a stack is a linked list of fixed-size chunks (1 KB to start, 32 KB each after
  that), an overflow links a new chunk in with the top kilobyte copied across for slack, an
  emptied chunk is dropped, and growth is O(1) with no copy at all. The same shape for the
  Unison stack would make growth cheap for the interpreter and remove the unwind for native
  code, since a native function whose frame doesn't fit could keep running in a new chunk
  rather than exiting. What it costs: slot addressing. MCode addresses slots as offsets from
  `sp` and native code as offsets from `fp`, and both assume the frame is contiguous; a
  frame would have to live entirely in one chunk (a function's maximum frame size is known,
  so an entry check can guarantee that, moving to a new chunk when it won't fit), and
  anything that walks the stack across frames (`Capture`, `saveFrame`'s copies, the
  continuation code, the GC marking in `unison_jit_enter`, the C helpers that read argument
  slots) would have to follow chunk links. A sizeable interpreter change, not a JIT one.
  Noted 2026-10-07 after a question about why growth isn't amortized.

- **Frame records for inline bindings.** A frame nested inside k inline `Let` bindings writes
  k + 1 records on unwind. Fine unless unwinds turn out to be frequent.
