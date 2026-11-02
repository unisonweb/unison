# 2026-11-02: known constructors and case of case

`unison-src/transcripts-manual/jit-benchmarks.md` on two optimized builds
(`.stack-work-opt`): 08a6fb73e (before) and the known-constructor and
case-of-case rewrites with join points (design.md "Known constructors and
case of case"), under `UNISON_JIT=on`, interleaved, best of three runs,
macOS arm64, the machine otherwise idle. Times are the transcript's own,
in milliseconds; the ratio is before over after, so above 1 is a gain.
Two rows were added for this measurement: a loop whose round matches on
two arguments at once (`match (Nat.mod i 3, Nat.mod i 5) with ..`, which
builds a tuple three packs deep and takes it apart), and a loop whose
round builds an `Optional` in the branches of a match and matches it at
once, with arms that bind a let, so that they are join points. The
interpreter was not measured in this run (the machine was needed);
the rewrites are on the ANF, so the interpreter runs the same code.

## The JIT, before and after

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.319 | 0.319 | 1.00x |
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.015 | 1.03x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.106 | 0.103 | 1.02x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 0.99x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 0.99x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.005 | 0.005 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.055 | 0.055 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.971 | 0.102 | 9.49x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.228 | 0.191 | 1.19x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.035 | 0.036 | 0.97x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.076 | 1.01x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.067 | 0.067 | 1.00x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.510 | 0.510 | 1.00x |
| Text: append "hi" 10000 times | 0.200 | 0.201 | 0.99x |
| Text: drop 1, 100000 times | 1.900 | 1.901 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.200 | 0.199 | 1.01x |
| Bytes: drop 1, 100000 times | 1.850 | 1.839 | 1.01x |
| Bytes: at, 100000 times | 2.150 | 2.097 | 1.03x |
| Text: uncons walk over 100000 characters | 3.274 | 3.289 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.289 | 0.289 | 1.00x |
| List: at, 10000 times | 0.154 | 0.154 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.708 | 0.717 | 0.99x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.043 | 0.043 | 1.00x |
| Ref.cas loop, 10000 times | 0.132 | 0.132 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.332 | 0.331 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 3.647 | 1.379 | 2.64x |
| Records: a 16-field literal and four getters, 100000 times | 5.058 | 0.096 | 52.94x |
| Counter: 100000 requests | 2.393 | 2.419 | 0.99x |
| Store: get and put, 100000 iterations | 5.456 | 5.559 | 0.98x |
| handle installed per iteration, 10000 iterations | 4.692 | 4.668 | 1.01x |
| Store whose put requests an outer Counter, 100000 iterations | 8.609 | 8.730 | 0.99x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.472 | 0.474 | 0.99x |

## What it showed

- **A match on two arguments at once, 9.5x** (0.97 ms to 0.10 ms for
  100000 rounds): the tuple and its inner pair and unit are not built and
  the three data matches are gone, leaving two numeric matches. This is
  the shape Paul expected the rewrite to help, and it was paying an
  allocation of three objects and three tag tests per round.
- **The record rows, 2.6x and 53x**: `recordLoop` builds a record
  literal and reads it by a pattern and by getters in the same body, and
  `wideLoop` builds a 16-field literal and reads four fields; the getters
  are inlined into field reads of the literal, which the known-constructor
  rule turns into the fields themselves, so the 16-field row allocates
  nothing and reads nothing (5.06 ms to 0.10 ms). Not written for this
  measurement; a shape library code has (a record built and consumed in
  one function).
- **The Optional built in branches, 1.19x** (0.228 ms to 0.191 ms): the
  outer match is gone and each branch calls the arm's join point in
  registers; the remaining cost is the loop itself. Under the wide
  convention this row already allocated nothing (an `Optional` travels in
  slots at K = 1), so the gain is the tag test and the slot traffic.
- **Everything else within noise**, the handler rows and the Collatz rows
  included: the join points and the dropped lets change nothing a hot loop
  of the suite did.

## Not measured yet

The interpreter rounds and the library suite (`jit-suite.md`); to be run
on the next optimized build.
