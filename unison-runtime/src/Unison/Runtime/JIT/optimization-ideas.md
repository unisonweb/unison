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
interpreter's `buildData` would read a per-cache table of them. How much it is worth: a
census of the interpreter on 2026-10-27 counted nullary constructors as 13.0M of the 20.1M
`Pack`s of `jitSuite` and 17.8M of the 28.5M of the library `suite` (the wide calling
convention, design.md "The wide calling convention", keeps the ones that are returned or
bound and never escape, about a quarter, out of the heap; the rest escape into structures
and still allocate). Native code already shares them: a `Pack` with no fields is a load of
the pool's enum for that constructor, so the gain would be the interpreter's.
Measured on 2026-10-28 with a per-cache table by type and constructor number,
filled as constructors were first built, consulted by every `Pack` with no
fields: no row of the benchmark suite moved by more than 2% under the
interpreter, a few were slightly slower. A three-word allocation in the
nursery costs about what the two array reads and the tag test cost, and the
garbage it leaves is free to a copying collector. Not kept; the entry stays
for the record. A `Box` of a witness with no fields still allocates its enum
natively, where the pool's entry for that constructor would do; rare, since a
nullary value that escapes is usually built boxed in the first place.

### The product-return worker: what is left

Built as design.md "Results exploded". Not done, in the order they are
likely to matter:

- A tail that never returns (`bug`, `Exception.raise`, an `abort`) makes
  the whole function keep its entry, since the analysis takes every tail
  to return something. An error path is common in real code; a tail that
  is a request or a call of a function that never returns could be
  allowed as a neutral tail.
- Sums: a function returning `Optional` or `Either` on every path could
  yield a tag word before the leaves, one width for all constructors;
  the wide convention covers the one-field ones today.
- A function that both takes a call pattern (a specialisation) and
  returns a product gets the return worker only for its entry's body: a
  specialisation's body still packs its result. A specialisation could
  get the yielding tails too (`yieldLeaves` on its body), as a worker
  per (pattern, result).
- A tail that calls a function returning the product through a *partial*
  shape (a wider product at the callee) is dropped rather than met down
  to what both build.
- A function gets its worker whether or not any caller takes the result
  apart (`Tree.insert`'s is never used); the worker could be emitted only
  when some site of the batch uses it, closed under the tail calls of the
  workers kept, which is also the rule an in-advance version needs.
- The workers are combinators at indices of their own (`specIndex`,
  `retIndex`), produced by `emitCombs` beside the group; they could be
  bindings of the group instead, so that the emitter saw one ordinary
  group, but the index scheme (the JIT's `isEntry`, the cells keyed by
  index) would have to be redone for it. Not worth it until something
  else needs the workers in the group.
- Load time: `specialise` prepares its body twice (before the argument
  rewrite, so that it maps variables by name, and after, for the products
  the rewrite builds), `explosions` prepares every spec it discovers once
  more for its sites, and `emitCombs` emits every lambda of a group twice,
  unoptimized for the frames of its sections and then rewritten. Each pass
  is linear in the body (an analysis enters only the subterms that mention
  the variable it asks about, `foldMentions`), and all of it together costs
  about 4% of the load of the suite's closure
  ([2026-11-02 known constructor](benchmarks/2026-11-02-known-constructor.md),
  "Load time over library code"); the unoptimized emission could be
  skipped for a group no rewrite touched if that ever matters.

### A private convention for specialised workers

A specialisation of a loop (`Unison.Runtime.MCode.Explode`) is called only
from sites the batch knows: its own body, its siblings, and the callers in
the batch. Inside one LLVM module those calls already go straight to the
`tailcc` workers with the arguments in registers; what stays uniform is
the wrapper that takes the arguments from the Unison stack, the entry poll,
the C-stack check and the exit write-back of a frame sized for the whole
function, all there for callers in other modules and for the interpreter.
With every caller known, the wrapper could be generated only when some
caller is in another module or interpreted, and a worker whose callers are
all in its module could skip reserving Unison-stack slots for its
arguments. The loops that matter compile into one
module, so the gain is in the entry and exit paths, to be measured.

### One witness closure

Native code tells a witness from an ordinary value by comparing the slot's
pointer against the pool's witness closures, one per field count
(`poolIndexWitness`; `wideK + 1` of them, so two with K = 1), since a
witness's info pointer is the one the type tags of unboxed numbers have.
So a `Box` of a value that may be a witness (the result of an unknown
call, say `f h` in a list map) is a chain of those compares, and a data
match on such a value has the same chain in front of its arms. With a
single witness closure, and the field count stored in the tag word in the
bits above the sixteen of the constructor number and the type number, both
become one compare; the interpreter would read the count from the word as
well (`WitnessTag` becomes a constant). Worth about the difference on the
`Cons.map` and `filterMap` rows (0.7x and 0.77x on 2026-10-28), where the
test is most of what the convention adds.

### A let over a call of a zero-arity definition

The benchmark row "Apply a function argument 10000 times" exits once per
iteration in both modes (13.8M exits in a run, 2026-10-28): its loop binds the
result of `App (Env f) ZArgs` where `f` is a zero-arity definition that is not
a cached constant, and `genLet` has no case for that binding, so the function
resumes in the interpreter at the Let every time. A call of a zero-arity
combinator is an ordinary call with no arguments; the generator should treat
it as one (worker call or uniform call), as it does `Call`. The row measures
the exit, not the application.

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

### Caching compiled code across runs

Definitions are content-addressed, so a hash of a module's definitions is a cache key, and
LLVM's JIT takes an object cache. On a program's second run the compile time would be near
zero. The obstacle is that generated code bakes in run-specific values: exit and frame table
indices, constant pool indices, and the addresses of native cells. Those would have to become
indirections or relocations before an object could be reused. The one change that would make
compile time stop mattering rather than smaller.

### Inlining small definitions before the rewrites

A loop that calls an accessor on its state each round keeps the accessor's match: with
`Slice.sum s = go i acc = if i < Slice.size s then go (i + 1) (acc + Slice.at i s) else acc`
over `structural type Slice a = Slice [a] Nat Nat`, the MCode of `go` is a call of
`Slice.size`, a call of `Slice.at` and the recursive call, and each callee matches on
the slice (`UNISON_JIT_DUMP_MCODE=1` on the example, 2026-11-05). The known-constructor
rewrite works within one body and the explosion gives a worker only to a function whose
body takes the argument apart, and `go` only passes `s` on, so neither fires, rightly:
exploding `s` would mean repacking it for the two calls. The runtime has no inliner
across definitions (the ANF only inlines variable aliases, `inlineAlias`), which is the
step GHC takes first. An ANF-level inliner for small non-recursive definitions, run
before `Unison.Runtime.MCode.Explode`, would make this case fall out: once `Slice.size`
and `Slice.at` are in `go`'s body the matches are on `s`, `go` gets a worker over the
three fields, the matches resolve against the known constructor and go, and a round is
the bounds test, the index and the match on the element; the `{Exception}` on `Slice.at`
is a request on the error path only. The threshold is a size in ANF nodes, the way the
JIT's own `judge` estimates; a definition inlined into a group changes that group's
continuations, so the deoptimization tables have to cover it like any other rewrite.

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

The generator builds each module as text (`Unison.Util.Text`, strict and chunked, no
longer `String`), megabytes of it per module, then converts it to a `Data.Text` and to
UTF-8 for LLVM's parser (`UT.toText` in Compile.hs, `encodeUtf8` in LLVM.hs), two more
copies. That is most of what the compile thread allocates, and the allocation is shared
with the running program (more GCs, major ones sooner). Building the module through
LLVM's C API, or as bitcode, would be a slight improvement.

But parsing IR is very fast, about 1% of a module's compile time ([2026-10-09 O0](benchmarks/2026-10-09-o0.md)), so this doesn't really seem like a big win.

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

### `ghccc` for every JIT-compiled function

Compile every Unison function under GHC's own calling convention (`ghccc`,
which LLVM has for GHC's LLVM backend: the STG registers, including the
heap and stack pointers and the allocation limit, are pinned to machine
registers and arguments travel in the rest) instead of the C convention as
the lingua franca between Haskell, the uniform entries and the workers. One
`prim` foreign function, written in Cmm and perhaps one per arity, takes the
pointer of the JIT-compiled function to call and its arguments (or just how
many elements to take from the Unison stack) and jumps to it; native code
then has the GC registers directly, so a call-out need not spill and reload
them through `Ctx`, allocation is the heap-pointer bump GHC's own code does,
and cross-module calls can return their values in registers instead of
through the Unison stack and the uniform entry's store and reload (Collatz
0.82x and the hot caller 0.80x under the wide convention,
`benchmarks/2026-10-28-wide-jit.md`, are that cost). What to establish
first: that `ghccc` and `musttail` agree on each target (an `llc` check of
a small module per target, as was done for the wide return), how a Cmm
`prim` enters a function pointer with the arguments where
`ghccc` wants them, and what the exits and the write-back become when native
code holds the STG registers rather than a `Ctx`.

Where the gains would come from, and in what order to go after them
(2026-10-31):

- The cross-module cost is not the C convention's. Workers in one module
  already call each other under `tailcc` in registers; a call through a
  cell took the uniform entry because the target may be interpreted or a
  worker of another width. Done on 2026-10-31 as a subset of this idea: a
  per-arity register signature agreed across modules, in a second word of
  the cell, with the stack form kept for interpreted targets and for
  functions without a worker (design.md, "The register entry"). What is
  left of this bullet: calls to function values still go through the
  closure's cell and the uniform entry, and could take the same second
  word.
- Pinned Hp and HpLim save the loads of the context's copy of the free
  pointer on each allocation: L1 hits, small.
- The structural gain is native code becoming Haskell code to the RTS:
  entered by a jump, exiting by a jump instead of unwinding to the
  trampoline, calling Haskell instead of calling out. The price is that a
  call which may collect sees the live boxed values: either STG frames with
  info tables the collector walks, generated by the JIT, or the discipline
  exits follow today, store the live values to their Unison stack slots (a
  root) before the call and reload them after, since the collector moves
  objects. The second needs no new frame format and costs stores and
  reloads at calls into Haskell, the write-back without the unwinding;
  whether that beats an exit depends on how many live values a typical
  call site holds, which the exit counters (`UNISON_JIT_STATS`) can
  attribute. There is no way to hold the collector off for a running
  thread; the unsafe foreign call native code runs in today is the nearest
  thing, and is why it never calls Haskell.
- Every C helper called from `ghccc` code must save and restore the pinned
  registers, as GHC's foreign calls do; the IR helpers could be `ghccc`
  themselves, the C ones (boxing, naming, text and bytes) cannot.

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

### Linear re-entry code for all cases

A re-entry function is the whole suffix of its function from its point on, generated
inline, so a function whose n re-entry points in sequence all get generated holds about
n²/2 of its instructions. In `on` mode that doesn't happen: certain call-outs are linear
(the parent stops at the call-out and each segment appears once), and the cap on ad hoc
re-entry functions (`UNISON_JIT_REENTRY_ADHOC`, 2026-10-20) bounds what the poll and stack
growth can ask for.

`eager` mode generated every Let body by design until 2026-11-06, when the wide convention's
larger suffixes made the test transcript's largest definition 95 MB of IR and the merge queue's
jit jobs three to five times longer; it now splits a group as `on` does and generates a body at
its first miss ([benchmarks/2026-11-06-eager-reentry.md](benchmarks/2026-11-06-eager-reentry.md)).
What remains is a suffix per Let whose callee calls out, each the function's whole rest: in
the test transcript the runner's own term for a stanza's watch expressions
(`Runtime.evaluateWatches`, a let rec whose body is a tuple of every watch; the Records
section's 43 watches get a suffix of 2.5 MB for each of the 5 that call out, since the
interpreter enters a body as soon as eager has served it), a shape rare in user code and the deferred copies after
slow-path call-outs (needs many sites of one function to keep taking their slow paths).

If one of those ever matters, the fix is to generate a re-entry function as its own binding
followed by a call through the *next* Let's (or deferred site's) cell instead of inlining the
rest: a direct call when that cell is compiled, otherwise the exit the interpreter would
take on an empty cell. Only the root function needs the straight inline path; the re-entry
path is cold.

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

## The "Mutate a local Remote.Ref 10k times" row ran at 0.58x of trunk (done 2026-11-05)

Resolved: the handler applied a four-argument function to three after a `Box` of a wide
variable, and `genApp` had no under-application case, so the binding left native code 10,000
times per run (the Let case only covered a binding that was the application alone, and trunk
has no boxes). Built in place from `genApp` now; the row is 1.13x of trunk
([benchmarks/2026-11-05-ref-row-partial-application.md](benchmarks/2026-11-05-ref-row-partial-application.md)).
What is left of the row is the handler's requests themselves (`SetAff`, the frame records at
each request), measured nowhere yet.

