# 2026-10-18: a compiled program against the same code from the codebase

`run.compiled` goes through the JIT from this day: `restoreCache` attaches native code cells,
`runStandalone` starts the JIT, and the `.uc` file carries the data types' constructor
arities ([Where cells live](../design.md#where-cells-live)). The question was whether a
compiled program runs as fast as the same code from a codebase. Optimized build
(`.stack-work-opt`), `--jit on`, one run of `jit-benchmarks.md` through `transcript.fork` on
`jit_codebase` (the "From the codebase" column), and one run of `jitSuite` compiled from that
codebase with `compile jitSuite jit-suite` and run with `run.compiled jit-suite.uc`.

| From the codebase | Compiled | Change | Benchmark |
| --- | --- | --- | --- |
| 322.773µs | 314.812µs | 0.98× | Sum 0 to 1 million |
| 35.247µs | 35.528µs | 1.01× | fib 20 |
| 13.563µs | 13.719µs | 1.01× | Cons list: map with a lambda (1000 elements) |
| 4.803µs | 4.841µs | 1.01× | Cons list: foldLeft with a lambda (1000 elements) |
| 104.3µs | 100.81µs | 0.97× | Binary tree: 1000 inserts |
| 38.14µs | 37.99µs | 1.00× | Binary tree: 1000 lookups |
| 57.116µs | 57.743µs | 1.01× | Apply a function argument 10000 times |
| 41.632µs | 40.373µs | 0.97× | Mutate a Ref 10000 times |
| 70.349µs | 70.339µs | 1.00× | Calls across definitions: Collatz steps for 1 to 1000 |
| 116.997µs | 118.721µs | 1.01× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 50.095µs | 198.533µs | 3.96× | Text: append "hi" 10000 times (see below) |
| 1.9128ms | 1.915209ms | 1.00× | Text: drop 1, 100000 times |
| 195.2µs | 194.292µs | 1.00× | Bytes: append 2 bytes 10000 times |
| 1.799871ms | 1.784349ms | 0.99× | Bytes: drop 1, 100000 times |
| 2.13172ms | 2.140615ms | 1.00× | Bytes: at, 100000 times |
| 3.305981ms | 3.328865ms | 1.01× | Text: uncons walk over 100000 characters |
| 284.102µs | 287.15µs | 1.01× | List: cons 10000 elements, then a pattern walk |
| 148.452µs | 143.899µs | 0.97× | List: at, 10000 times |
| 708.944µs | 707.361µs | 1.00× | Nat.toText and Nat.fromText, 10000 times |
| 1.008403ms | 1.004092ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 489.319µs | 489.964µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 539.535µs | 540.306µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 46.151µs | 45.383µs | 0.98× | MutableArray: fill, freeze and sum 10000 elements |
| 127.715µs | 127.428µs | 1.00× | Ref.cas loop, 10000 times |
| 277.521µs | 272.86µs | 0.98× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.92319ms | 7.702175ms | 0.97× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.171475ms | 4.970056ms | 0.96× | Records: a 16-field literal and four getters, 100000 times |
| 3.3799ms | 3.33764ms | 0.99× | Counter: 100000 requests |
| 6.332712ms | 6.315928ms | 1.00× | Store: get and put, 100000 iterations |
| 4.271418ms | 4.282581ms | 1.00× | handle installed per iteration, 10000 iterations |
| 11.245675ms | 10.231496ms | 0.91× | Store whose put requests an outer Counter, 100000 iterations |
| 470.166µs | 455.691µs | 0.97× | Bytes: decodeNat64be walk over 80000 bytes |

A compiled program runs as the same code does from the codebase: every row is within the
suite's run-to-run noise, and the compile log shows the same modules compiled. The text
append row is not a difference between the two paths: run twice more each way it gave
200.6 µs and 50.2 µs from the codebase and 200.1 µs and 247.1 µs compiled, so the row is
bimodal (50 µs or 200 to 250 µs) on both paths, which the records of 2026-10-16 and
2026-10-17 (239 and 203 µs) had not shown. Something to look into; noted in
[optimization-ideas.md](../optimization-ideas.md).
