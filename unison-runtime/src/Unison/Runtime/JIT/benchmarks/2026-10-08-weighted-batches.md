# The weighted batch rule (2026-10-08)

`formBatch` became Prim's algorithm over the call graph, with edges weighted by estimated calls
and a gate of N/4 ([design](../design.md#what-gets-compiled-and-when)), replacing the
breadth-first walk with its N/2 (callee) and one-call (caller) gates. These runs compare the
two rules on the same optimized build, `--jit on`, one run each, no statistics; the build had
both rules behind `UNISON_JIT_BATCH_RULE` for the comparison, and the breadth-first one was
removed afterwards. A new row, "callee first reached once the caller is hot", was added to
`jitSuite` for this: its callee is not reached during the first 20,000 iterations, so the
breadth-first rule, which judges a callee by its own count, leaves it out of the caller's
batch, and the caller calls it through its cell for the rest of the run.

`jitSuite`:

| Benchmark | breadth-first | weighted | weighted/breadth |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 322 µs | 321 µs | 1.00× |
| fib 20 | 41.7 µs | 41.4 µs | 0.99× |
| Cons list: map with a lambda (1000 elements) | 14.6 µs | 13.9 µs | 0.95× |
| Cons list: foldLeft with a lambda (1000 elements) | 4.95 µs | 4.88 µs | 0.99× |
| Binary tree: 1000 inserts | 202 µs | 194 µs | 0.96× |
| Binary tree: 1000 lookups | 54.4 µs | 51.5 µs | 0.95× |
| Apply a function argument 10000 times | 48.8 µs | 48.7 µs | 1.00× |
| Mutate a Ref 10000 times | 35.1 µs | 36.1 µs | 1.03× |
| Calls across definitions: Collatz steps for 1 to 1000 | 68.2 µs | 67.5 µs | 0.99× |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 353 µs | 119 µs | 0.34× |
| Text: append "hi" 10000 times | 255 µs | 258 µs | 1.01× |
| Text: drop 1, 100000 times | 2.21 ms | 2.25 ms | 1.02× |
| Bytes: append 2 bytes 10000 times | 243 µs | 257 µs | 1.06× |
| Bytes: drop 1, 100000 times | 2.29 ms | 2.18 ms | 0.95× |
| Bytes: at, 100000 times | 2.28 ms | 2.23 ms | 0.98× |
| Text: uncons walk over 100000 characters | 4.2 ms | 3.97 ms | 0.94× |
| Nat.toText and Nat.fromText, 10000 times | 728 µs | 727 µs | 1.00× |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 997 µs | 996 µs | 1.00× |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 4.28 ms | 4.29 ms | 1.00× |
| MutableArray: fill, freeze and sum 10000 elements | 47.1 µs | 46.4 µs | 0.99× |
| Ref.cas loop, 10000 times | 120 µs | 122 µs | 1.02× |
| murmurHashUntyped of Some (i, "x"), 10000 times | 268 µs | 270 µs | 1.01× |
| Bytes: decodeNat64be walk over 80000 bytes | 460 µs | 465 µs | 1.01× |

Everything but the new row is within the suite's run-to-run noise (about 10%). The new row is
3× faster: under weighted the callee joins the batch on the strength of its caller's count
(one site, once per call), the call is direct and LLVM inlines it.

What the rules compiled, from `UNISON_JIT_STATS`, both repeated across two runs each:

| | breadth-first | weighted |
| --- | --- | --- |
| modules | 54 | 68 |
| functions compiled | 121 | 109 |
| re-entry functions never asked for | 311 | 193 |
| IR | 10.2 MB | 9.1 MB |
| compile time | 3.7 s | 2.4 s |
| largest module | 6 definitions, 2.1 MB of IR, 1.4 s | 24 definitions, 2.4 MB, 200 ms |

The compile time is the breadth-first rule taking callers with a single call (anything up the
chain to `main`), which cost a 1.4 s module around one definition that weighted compiles alone
in 10 ms. The weighted rule's modules are more numerous and smaller, apart from two large ones
(24 and 15 definitions) that are the connected hot sets of the suite's harness. (The counts
above are from the suite before the new row was added. With it, the weighted rule's largest
module grows to 35 functions of 3.7 MB with 7 private copies, 1.2 s to compile, and the total
to about 3.5 s: a connected hot set is as large as the gate lets it be, and the cap B counts
definitions, not functions. A bound on a module's size in IR, or compiling the trigger alone
first, is the open point here; see [optimization-ideas](../optimization-ideas.md).)

Two things learned on the way, both recorded in [optimization-ideas](../optimization-ideas.md):
Unison's own compiler inlines a small callee into a tail call, so a benchmark of
cross-definition calls needs the call out of tail position; and the batch forms about a
millisecond after the trigger, which on the optimized build is a thousand or more interpreted
calls, so the first versions of the benchmark (callee on one path in three, then one in ten)
were already being called by the time the batch looked.
