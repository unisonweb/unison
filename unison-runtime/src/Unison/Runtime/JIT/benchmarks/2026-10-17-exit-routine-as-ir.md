# 2026-10-17: the exit routine as IR, with an argument block

The write-back routine `unison_jit_exit_frame` moved from C in `jit_rt.c` (clang's
`preserve_most` attribute, variadic) to `cbits/jit_exit.c`, compiled to IR at development
time and by the JIT at startup, with the live slots passed through a per-function
`alloca` instead of variadic arguments ([Write-back](../design.md#write-back)). The
question was whether the stores into the block cost anything on the rows where exits and
unwinds are frequent. Optimized build (`.stack-work-opt`), `--jit on`, one run of
`jit-benchmarks.md` on 2026-10-17, against the `--jit on` column of
[2026-10-16 against trunk](2026-10-16-against-trunk.md) (same machine).

| 2026-10-16 | 2026-10-17 | Change | Benchmark |
| --- | --- | --- | --- |
| 319 µs | 317.694µs | 1.00× | Sum 0 to 1 million |
| 35.3 µs | 35.622µs | 1.01× | fib 20 |
| 13.4 µs | 13.758µs | 1.03× | Cons list: map with a lambda (1000 elements) |
| 4.85 µs | 4.85µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 104 µs | 106.466µs | 1.02× | Binary tree: 1000 inserts |
| 38.3 µs | 39.154µs | 1.02× | Binary tree: 1000 lookups |
| 58 µs | 57.82µs | 1.00× | Apply a function argument 10000 times |
| 41.8 µs | 41.894µs | 1.00× | Mutate a Ref 10000 times |
| 66.3 µs | 66.994µs | 1.01× | Calls across definitions: Collatz steps for 1 to 1000 |
| 119 µs | 118.089µs | 0.99× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 239 µs | 203.452µs | 0.85× | Text: append "hi" 10000 times |
| 1.88 ms | 1.89024ms | 1.01× | Text: drop 1, 100000 times |
| 237 µs | 196.655µs | 0.83× | Bytes: append 2 bytes 10000 times |
| 1.82 ms | 1.881348ms | 1.03× | Bytes: drop 1, 100000 times |
| 2.15 ms | 2.164231ms | 1.01× | Bytes: at, 100000 times |
| 3.36 ms | 3.338418ms | 0.99× | Text: uncons walk over 100000 characters |
| 296 µs | 294.171µs | 0.99× | List: cons 10000 elements, then a pattern walk |
| 143 µs | 148.98µs | 1.04× | List: at, 10000 times |
| 719 µs | 719.159µs | 1.00× | Nat.toText and Nat.fromText, 10000 times |
| 1 ms | 1.010067ms | 1.01× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 500 µs | 487.652µs | 0.98× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 539 µs | 540.916µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 45.7 µs | 46.953µs | 1.03× | MutableArray: fill, freeze and sum 10000 elements |
| 132 µs | 130.883µs | 0.99× | Ref.cas loop, 10000 times |
| 283 µs | 281.476µs | 0.99× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.45 ms | 7.845853ms | 1.05× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.25 ms | 5.083103ms | 0.97× | Records: a 16-field literal and four getters, 100000 times |
| 3.3 ms | 3.384946ms | 1.03× | Counter: 100000 requests |
| 6.67 ms | 6.421243ms | 0.96× | Store: get and put, 100000 iterations |
| 4.26 ms | 4.326956ms | 1.02× | handle installed per iteration, 10000 iterations |
| 10.5 ms | 10.318821ms | 0.98× | Store whose put requests an outer Counter, 100000 iterations |
| 472 µs | 457.224µs | 0.97× | Bytes: decodeNat64be walk over 80000 bytes |

Every row is within the run-to-run noise of the suite (a few percent) except the two append
rows, 15% faster, which is the slack-chunk change of 2026-10-17
([slack chunks](2026-10-17-slack-chunks.md)) landing after the 2026-10-16 run, not this
change; the four rows that motivated `preserve_most` (Collatz, callee first reached once
the caller is hot, the Counter and Store requests) are unchanged. The whole test matrix passed on the fast build
and its first four rows on this build before the run.
