# 2026-10-19: the JIT's state in one handle

Review of the PR asked why the JIT kept its state in globals (`unsafePerformIO`
IORefs for the tables, the queue, the compiler's memos, and the settings read from
the environment by a CAF). Now everything lives in one `JIT` record (JIT.hs), made
by ucm at startup and carried by every code cache (`CCache`'s `jit` field), which is
how the interpreter reaches it. The question was what the interpreter's hot paths
pay for reading through the handle instead of a top-level constant: `enter` checks
the tracing switch on every interpreted combinator entry, and the trampoline reads
the constant pool, the exits table and the frames table on every round trip.

Optimized build (`.stack-work-opt`), `--jit on`, `jit-benchmarks.md` through
`transcript.fork`, against the 2026-10-17 column of
[2026-10-17 exit routine as IR](2026-10-17-exit-routine-as-ir.md) (same machine). The
"first" column is the refactoring as first written, with the trampoline reading the
three tables through the handle on every round trip (cache, handle, compiler record,
table: three dependent loads more than before). The "hoisted" column reads them once
per `runNative` call instead, next to the two switches that were already read there.
Each column is one run; two further runs of the hoisted build agreed with it, and a
run of the suite takes about five minutes at this setting.

| 2026-10-17 | first | hoisted | hoisted / 2026-10-17 | Benchmark |
| --- | --- | --- | --- | --- |
| 317.694µs | 319.036µs | 319.266µs | 1.00× | Sum 0 to 1 million |
| 35.622µs | 35.801µs | 35.399µs | 0.99× | fib 20 |
| 13.758µs | 13.797µs | 13.79µs | 1.00× | Cons list: map with a lambda (1000 elements) |
| 4.85µs | 4.825µs | 4.824µs | 0.99× | Cons list: foldLeft with a lambda (1000 elements) |
| 106.466µs | 98.32µs | 98.552µs | 0.93× | Binary tree: 1000 inserts |
| 39.154µs | 38.333µs | 38.049µs | 0.97× | Binary tree: 1000 lookups |
| 57.82µs | 58.568µs | 57.849µs | 1.00× | Apply a function argument 10000 times |
| 41.894µs | 41.645µs | 41.216µs | 0.98× | Mutate a Ref 10000 times |
| 66.994µs | 70.71µs | 66.467µs | 0.99× | Calls across definitions: Collatz steps for 1 to 1000 |
| 118.089µs | 119.383µs | 118.579µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 203.452µs | 204.527µs | 198.38µs | 0.98× | Text: append "hi" 10000 times |
| 1.89024ms | 1.887302ms | 1.874872ms | 0.99× | Text: drop 1, 100000 times |
| 196.655µs | 197.693µs | 194.988µs | 0.99× | Bytes: append 2 bytes 10000 times |
| 1.881348ms | 1.821193ms | 1.793789ms | 0.95× | Bytes: drop 1, 100000 times |
| 2.164231ms | 2.161855ms | 2.150903ms | 0.99× | Bytes: at, 100000 times |
| 3.338418ms | 3.260371ms | 3.262471ms | 0.98× | Text: uncons walk over 100000 characters |
| 294.171µs | 292.92µs | 285.834µs | 0.97× | List: cons 10000 elements, then a pattern walk |
| 148.98µs | 148.925µs | 151.678µs | 1.02× | List: at, 10000 times |
| 719.159µs | 716.502µs | 712.456µs | 0.99× | Nat.toText and Nat.fromText, 10000 times |
| 1.010067ms | 1.006007ms | 1.004107ms | 0.99× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 487.652µs | 487.205µs | 490.199µs | 1.01× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 540.916µs | 541.066µs | 539.235µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 46.953µs | 44.72µs | 44.425µs | 0.95× | MutableArray: fill, freeze and sum 10000 elements |
| 130.883µs | 127.117µs | 126.702µs | 0.97× | Ref.cas loop, 10000 times |
| 281.476µs | 278.094µs | 274.341µs | 0.97× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.845853ms | 7.699165ms | 7.415562ms | 0.95× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.083103ms | 5.837618ms | 5.033053ms | 0.99× | Records: a 16-field literal and four getters, 100000 times |
| 3.384946ms | 3.489115ms | 3.358153ms | 0.99× | Counter: 100000 requests |
| 6.421243ms | 6.376831ms | 6.259456ms | 0.97× | Store: get and put, 100000 iterations |
| 4.326956ms | 4.423043ms | 4.439521ms | 1.03× | handle installed per iteration, 10000 iterations |
| 10.318821ms | 10.276312ms | 10.210412ms | 0.99× | Store whose put requests an outer Counter, 100000 iterations |
| 457.224µs | 460.83µs | 463.273µs | 1.01× | Bytes: decodeNat64be walk over 80000 bytes |

With the reads hoisted every row is within noise of 2026-10-17. Before the hoist the
one row that moved was the 16-field record benchmark, 12 to 15% slower (5.84 and 5.68
ms in two runs against 5.08 before): it makes a round trip between native code and
the interpreter per iteration, so it paid the three loads each time. The rows that
are interpreted throughout ("handle installed per iteration") did not move in either
column, so `enter`'s extra loads for the tracing switch cost nothing measurable.
