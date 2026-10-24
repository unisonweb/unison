# Benchmark runs

One file per run, named `<yyyy-mm-dd>-<what-it-measured>.md`: what the run was (build, mode,
how many runs), the table, then what it showed. How to run the suites is in
[development.md](../development.md). The latest full run is what the next one is compared
against.

| Run | What it answered |
| --- | --- |
| [2026-09-29 interpreter baseline](2026-09-29-interpreter-baseline.md) | both suites with no JIT code at all |
| [2026-09-29 cell-check overhead](2026-09-29-cell-check-overhead.md) | what the native code cell costs the interpreter (under 2%) |
| [2026-09-30 M1 to M5 steps](2026-09-30-m1-to-m5-steps.md) | `jitSuite` after each early milestone, and what still exited |
| [2026-10-01 suite after M5](2026-10-01-suite-after-m5.md) | the broad suite's first run with the JIT: 24 rows slower, and the 107 M exits by kind |
| [2026-10-01 M6 exit rule and workers](2026-10-01-m6-exit-rule-and-workers.md) | each M6 step: the static exit rule, the round trip from 67 to 37 ns, workers and what each change bought `fib` |
| [2026-10-02 Deque vs Data.Sequence](2026-10-02-deque-vs-data-sequence.md) | the strict finger tree against the list it replaced |
| [2026-10-02 rope vs old rope](2026-10-02-rope-vs-old-rope.md) | the finger-tree rope against the size-balanced tree, and the threshold |
| [2026-10-02 native lists in the suite](2026-10-02-native-lists-in-the-suite.md) | the suite on the strict list with native list operations |
| [2026-10-02 text and bytes helpers](2026-10-02-text-and-bytes-helpers.md) | the text and bytes rows as they went native, and the open `Json.toText` loss |
| [2026-10-03 every benchmark](2026-10-03-every-benchmark.md) | full run midway through M7 |
| [2026-10-03 inline bump allocation](2026-10-03-inline-bump-allocation.md) | bump allocation, then the budget folded into the limit |
| [2026-10-04 private copies and re-entry batches](2026-10-04-private-copies-and-reentry-batches.md) | compile totals and the suite with copies and batched re-entry functions |
| [2026-10-04 floats and hash](2026-10-04-floats-and-hash.md) | the float loop and `murmurHashUntyped` |
| [2026-10-05 arrays and refs](2026-10-05-arrays-and-refs.md) | the array, ref and CAS rows |
| [2026-10-05 every benchmark, end of M7](2026-10-05-every-benchmark-end-of-m7.md) | the closing tables of M7 |
| [2026-10-07 LLVM loaded at run time](2026-10-07-llvm-loaded-at-run-time.md) | every benchmark after the switch to `dlopen`; unchanged from the end of M7 |
| [2026-10-08 weighted batches](2026-10-08-weighted-batches.md) | the weighted batch rule against the breadth-first walk, and the benchmark that separates them |
| [2026-10-09 compile thread wake-up](2026-10-09-compile-thread-wake-up.md) | request-to-batch lag, with the compile thread on its own capability |
| [2026-10-09 compile time](2026-10-09-compile-time.md) | where a module's compile time goes; LLVM's machine schedulers off and a size term in the batch rule halve it |
| [2026-10-09 write-back](2026-10-09-write-back.md) | exits share write-back blocks and write only live slots: IR 6.9 → 3.8 MB, compile time 1.75 → 1.26 s |
| [2026-10-09 O0](2026-10-09-o0.md) | the optimization passes and the backend each cost about half the compile time, and each is worth 5× or more at run time |
| [2026-10-09 pipeline](2026-10-09-pipeline.md) | a pass pipeline that produces the same code as `default<O2>` on the suite in 60% of the pass time; what each left-out pass turned out to do |
| [2026-10-09 exit call](2026-10-09-exit-call.md) | exits write the frame back through one C call per site instead of generated blocks: IR 3.8 → 2.9 MB, compile time 1.03–1.14 → 0.88–0.93 s, hot code unchanged or faster; why not `llvm.experimental.deoptimize` |
| [2026-10-10 backend profile](2026-10-10-backend-profile.md) | where code generation's time goes: instruction selection, register allocation and the machine passes in proportion to code; no fixed cost, no single pass |
| [2026-10-10 backend options](2026-10-10-backend-options.md) | GlobalISel and the basic register allocator: each saves 7–10% of code generation and costs the hot loops 4–8%; not taken |
| [2026-10-10 helpers](2026-10-10-helpers.md) | byte array sizes, reads and writes generated inline (the array row 4.18 → 0.49 ms); murmur compiled from C to an IR module the JIT adds at startup, so no C helper returns a two-word pair; exporting the layout tables cost the rope rows 6% and was undone |
| [2026-10-11 requests](2026-10-11-requests.md) | ability requests of affine handlers native: Counter 16.2 → 3.81 ms, Store 32.8 → 9.44 ms; 35 ns per request against 160; other rows unchanged, 23 more functions compile |
| [2026-10-11 local](2026-10-11-local.md) | handler cases that request an outer ability run natively: Store with an outer Counter request 38.3 → 10.4 ms, Ask relayed 3 deep 37.2 → 10.3 ms; nesting past 8 handlers costs one call-out per request |
| [2026-10-11 ptr](2026-10-11-ptr.md) | FFI.Ptr reads and writes inline: 28.9 ms → 0.54 ms for 100000 iterations of ten accesses; an interpreter bug in the sized Int conventions fixed |
| [2026-10-11 replacements](2026-10-11-replacements.md) | the Haskell replacements of base definitions against native code: the `Map` family and `List.range` go native under the JIT (insert 156 → 58 ns, lookup 123 → 13 ns at 10 keys), `List.sort`, JSON, Avro and the `Bytes.read*`s stay replaced; then universal comparison made native for every value, `Map.==` natively 1197 → 590 ns per key |
| [2026-10-15 records](2026-10-15-records.md) | structural records native: the records row 91.1 ms → 7.9 ms (910 → 79 ns per iteration of six record operations); records as exits cost what the interpreter does |
| [2026-10-15 positions](2026-10-15-positions.md) | a shape's field positions as two sorted unboxed arrays instead of an IntMap: the records row 7.9 → 7.5 ms; a new 16-field row at 5.24 ms native, 73.4 ms interpreted |
| [2026-10-15 data layout](2026-10-15-data-layout.md) | constructors with 3 to 6 fields flat, segments without boxes: tree inserts 0.189 → 0.102 ms native and 1.69 → 1.30 ms interpreted; the rest unchanged |
| [2026-10-15 constructor order](2026-10-15-constructor-order.md) | the interpreter's dispatch counts per constructor, and the section, instruction, primitive and `Foreign` constructors reordered so the six most dispatched hold their own pointer tags; then `PAp` moved to tag 7 at no measurable cost |
| [2026-10-15 text list](2026-10-15-text-list.md) | texts and lists as closure constructors of their own (`TextC`, `ListC`, one object each, pointer tags 1 and 2) instead of two `Foreign` wrappers: text rows 4 to 8% faster native, 1 to 5% interpreted; `Data4` moved to tag 7, the tree rows 6 to 18% slower native; then `Data5` (base's `Map` node) measured against `Data3` for the sixth tag and taken: worth 2 to 5% on the Nat map rows against the same node at tag 7, nothing else moves; the chain of the day's four binaries and the Haskell call-out on the map rows |
| [2026-10-16 bytes constructor](2026-10-16-bytes-constructor.md) | the bytes rows with `Bytes` as a closure constructor at tag 7 (`BytesC`, one object): 8 to 14% faster native where a result is allocated, the text rows as control within noise |
| [2026-10-16 against trunk](2026-10-16-against-trunk.md) | both suites, trunk's binary against `--jit on`: 10 to 200× on loops, arithmetic and calls, 4 to 130× on data, text, bytes and arrays, the map construction and JSON rows below 1× |
| [2026-10-17 exit routine as IR](2026-10-17-exit-routine-as-ir.md) | the write-back routine as LLVM IR with an argument block instead of C with variadic arguments: every row within noise, including the four that motivated `preserve_most` |
| [2026-10-17 slack chunks](2026-10-17-slack-chunks.md) | a joined chunk's array over-allocated, the next append written in place after a CAS on the used length: append rows 16% faster native, 10% interpreted; the starting slack (8, 16, 32) makes no difference |
| [2026-10-18 compiled programs](2026-10-18-compiled-programs.md) | `run.compiled` through the JIT against the same code from the codebase: every row within noise; the text append row found bimodal (50 or 200 to 250 µs) on both paths |
| [2026-10-19 JIT handle](2026-10-19-jit-handle.md) | the JIT's state moved from globals into one handle carried by the code cache: every row within noise once the trampoline reads the three tables once per entry rather than per round trip (the 16-field record row paid 12% for the per-round-trip reads) |
| [2026-10-19 C split](2026-10-19-c-split.md) | jit_rt.c in one file per builtin family: compiled as separate units three rows lost 4 to 7% (calls across files that were inlined in one), as one translation unit (jit_rt.c includes the files) every row within noise; the latter is kept |
| [2026-10-19 unboxing](2026-10-19-unboxing.md) | the hand-unboxed rope code rewritten as ordinary pairs and measured on the package's benchmarks: the deque's unboxed tuples bought nothing (GHC unboxes those results itself) and are gone; the rope's chunk lookup keeps its unboxed form, the one place that measured 5 to 8% on taking a text apart a character at a time |
| [2026-10-20 ad hoc re-entry cap](2026-10-20-adhoc-reentry-cap.md) | the poll and stack growth no longer ask for a re-entry function at every Let they interrupt: at most `UNISON_JIT_REENTRY_ADHOC` (3) per definition, counted by the trampoline when it builds the frames; every row within noise, and the cap shown refusing under the poll stress |
| [2026-10-20 Name](2026-10-20-name.md) | the four-argument limit of `Name` lifted: a partial application of a known function built inline, one of a function value through the helper with its arguments on the Unison stack; the `Remote.Ref` row goes from a third slower than interpreted to two to three times faster, and the three ability-request rows gain 20% |
| [2026-10-21 match dispatch](2026-10-21-match-dispatch.md) | a data match dispatches on the shape using the type's constructor arities: only the shapes the type has, straight to the arm when a shape has one constructor, the packed tag loaded only where constructors share a shape; tree lookups 0.42×, inserts 0.88×, the rest within noise |
| [2026-10-21 List.range](2026-10-21-list-range.md) | the 2026-10-11 List.range rows never forced the result; forced, native is faster at 10 and 100000 elements and 25% slower at 1000, so it stays native |
| [2026-10-24 security checks](2026-10-24-security-checks.md) | the security review's runtime checks (closure call stack room from the copy's base, non-wrapping array bounds, the frame-record bound, may-fail allocation, pinnedness): every row within noise; the Ref row 0.83× from calling the MutVar barrier only for a clean variable; phase 2 (the MCode verifier, the wide-constructor tag compare) and phase 3 (acquire and release on the code cells) every row within noise |
