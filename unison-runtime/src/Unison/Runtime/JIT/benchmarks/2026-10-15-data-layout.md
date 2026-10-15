# 2026-10-15: flat data constructors

Constructors with 3 to 6 fields became flat closures (`GData3` … `GData6`), `DataG`, `PAp`
and `Captured` hold their two segment arrays directly, and the pointer tags were
reassigned ("Layouts" in internals.md; the plan is `data-layout-plan.local.md`). Optimized
builds of the commit before and of this one, the suite (`jit-benchmarks.md`) run twice per
binary and mode, alternating the binaries, best of the two per cell; the first row's label
is overwritten by the progress line in the console but the parser reads the log.

| row | interpreter before | interpreter after | JIT before | JIT after |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 66.1 ms | 70.2 ms | 0.315 ms | 0.316 ms |
| fib 20 | 1.4 ms | 1.46 ms | 0.0354 ms | 0.0354 ms |
| Cons list: map with a lambda (1000 elements) | 0.0866 ms | 0.0867 ms | 0.0135 ms | 0.013 ms |
| Cons list: foldLeft with a lambda (1000 elements) | 0.0865 ms | 0.0867 ms | 0.00486 ms | 0.00483 ms |
| Binary tree: 1000 inserts | 1.69 ms | 1.37 ms | 0.189 ms | 0.102 ms |
| Binary tree: 1000 lookups | 0.767 ms | 0.784 ms | 0.0461 ms | 0.0437 ms |
| Apply a function argument 10000 times | 1.16 ms | 1.2 ms | 0.0481 ms | 0.057 ms |
| Mutate a Ref 10000 times | 0.828 ms | 0.893 ms | 0.0407 ms | 0.0416 ms |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.79 ms | 8.19 ms | 0.0663 ms | 0.0672 ms |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 16.0 ms | 16.7 ms | 0.119 ms | 0.118 ms |
| Text: append "hi" 10000 times | 1.07 ms | 1.12 ms | 0.261 ms | 0.264 ms |
| Text: drop 1, 100000 times | 9.6 ms | 9.97 ms | 2.05 ms | 2.05 ms |
| Bytes: append 2 bytes 10000 times | 1.03 ms | 1.07 ms | 0.258 ms | 0.261 ms |
| Bytes: drop 1, 100000 times | 8.66 ms | 9.14 ms | 2.06 ms | 2.07 ms |
| Bytes: at, 100000 times | 10.1 ms | 10.4 ms | 2.2 ms | 2.23 ms |
| Text: uncons walk over 100000 characters | 10.9 ms | 12.8 ms | 3.61 ms | 3.58 ms |
| Nat.toText and Nat.fromText, 10000 times | 7.88 ms | 8.07 ms | 0.721 ms | 0.72 ms |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.4 ms | 30.9 ms | 1 ms | 1.01 ms |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 63.8 ms | 64.4 ms | 0.49 ms | 0.487 ms |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 28.9 ms | 29.2 ms | 0.54 ms | 0.54 ms |
| MutableArray: fill, freeze and sum 10000 elements | 3.9 ms | 4.08 ms | 0.0457 ms | 0.0452 ms |
| Ref.cas loop, 10000 times | 1.03 ms | 1.08 ms | 0.127 ms | 0.128 ms |
| murmurHashUntyped of Some (i, "x"), 10000 times | 6.7 ms | 7.41 ms | 0.277 ms | 0.282 ms |
| Records: a literal, an update, a pattern and two getters, 100000 times | 94.1 ms | 94.6 ms | 7.5 ms | 7.59 ms |
| Records: a 16-field literal and four getters, 100000 times | 73.0 ms | 73.3 ms | 5.12 ms | 5.09 ms |
| Counter: 100000 requests | 16.4 ms | 16.6 ms | 3.66 ms | 3.25 ms |
| Store: get and put, 100000 iterations | 27.1 ms | 27.5 ms | 6.72 ms | 6.6 ms |
| handle installed per iteration, 10000 iterations | 6.29 ms | 6.26 ms | 4.44 ms | 4.33 ms |
| Store whose put requests an outer Counter, 100000 iterations | 38.7 ms | 39.2 ms | 10.8 ms | 10.4 ms |
| Bytes: decodeNat64be walk over 80000 bytes | 1.5 ms | 1.66 ms | 0.482 ms | 0.516 ms |

What it shows:

- **Building a 4-field constructor** (the tree's `Node`) is where the change was aimed:
  inserts 0.189 → 0.102 ms native (one object of 11 words instead of five of 23, and no box
  checks when the node is matched) and 1.69 → 1.37 ms in the interpreter, which reads the
  slots into the constructor directly instead of through a segment. Lookups, which only
  read fields, gain a little (three loads per field became one).
- **Nothing else moved on the native side** within the run-to-run noise, after two
  regressions found on the way were fixed: a generic hash walker with a loop over a dynamic
  field count, inlined into `mh_val`, cost the hash row 40% and tripped LLVM's fixpoint
  self-check (straight-line code for the 0-, 1- and 2-field shapes with the wider ones out of
  line restored 0.277 → 0.282 ms; development.md has the lesson), and the first generic
  `dataBranch` allocated a closure per match, 15% on every interpreter row with an `if`.
- **Open**: "Apply a function argument" reads 0.048 → 0.057 ms native in all three
  measurement rounds, 0.9 ns per closure call. The closure-call code lost the two box checks
  and nothing else changed there; not explained. The interpreter's data-free rows read 3 to
  6% slower in the suite in every round too, but a standalone run of the same loop
  (`sumTo 1000000` twenty times, `+RTS -s`) allocates the same bytes and takes the same
  mutator time with both binaries, so that one is attributed to the suite's measurement
  context (one sample per row, after the other rows) rather than to the change.

The records rows are a control (records are their own closure kind) and are unchanged.
