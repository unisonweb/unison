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
