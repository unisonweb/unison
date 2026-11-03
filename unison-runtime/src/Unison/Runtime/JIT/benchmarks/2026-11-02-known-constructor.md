# 2026-11-02: known constructors and case of case

`unison-src/transcripts-manual/jit-benchmarks.md` on two optimized builds
(`.stack-work-opt`): 08a6fb73e (before) and the known-constructor and
case-of-case rewrites with join points (design.md "Known constructors and
case of case"), under `UNISON_JIT=on`, interleaved, best of three runs,
and under `UNISON_JIT=off` (the interpreter), best of two, macOS arm64,
the machine otherwise idle. Times are the transcript's own,
in milliseconds; the ratio is before over after, so above 1 is a gain.
Two rows were added for this measurement: a loop whose round matches on
two arguments at once (`match (Nat.mod i 3, Nat.mod i 5) with ..`, which
builds a tuple three packs deep and takes it apart), and a loop whose
round builds an `Optional` in the branches of a match and matches it at
once, with arms that bind a let, so that they are join points. The
rewrites are on the ANF, so the interpreter runs the rewritten code too.

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

## The interpreter, before and after

The after build is 27a130cad (the rewrites plus the table-free check for
unchanged sections, which changes nothing the interpreter runs).

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 63.600 | 63.483 | 1.00x |
| fib 20 | 1.250 | 1.273 | 0.98x |
| Cons list: map with a lambda (1000 elements) | 0.089 | 0.089 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.089 | 0.089 | 1.00x |
| Binary tree: 1000 inserts | 1.263 | 1.269 | 0.99x |
| Binary tree: 1000 lookups | 0.618 | 0.619 | 1.00x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.111 | 0.111 | 1.00x |
| Cons list: head, 10000 times | 1.125 | 1.128 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.894 | 0.899 | 1.00x |
| Ok or Err from a step function, 10000 steps | 1.454 | 1.462 | 0.99x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 6.370 | 6.394 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 10.642 | 10.632 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 10.632 | 10.635 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 2.515 | 2.527 | 0.99x |
| Known constructor: a match on two arguments at once, 100000 rounds | 14.618 | 10.591 | 1.38x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 14.024 | 11.512 | 1.22x |
| Apply a function argument 10000 times | 1.090 | 1.103 | 0.99x |
| Mutate a Ref 10000 times | 0.802 | 0.801 | 1.00x |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.093 | 7.133 | 0.99x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 15.652 | 15.727 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 6.559 | 6.596 | 0.99x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 23.197 | 23.340 | 0.99x |
| Text: append "hi" 10000 times | 0.895 | 0.897 | 1.00x |
| Text: drop 1, 100000 times | 9.036 | 9.069 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.869 | 0.874 | 0.99x |
| Bytes: drop 1, 100000 times | 8.186 | 8.193 | 1.00x |
| Bytes: at, 100000 times | 10.384 | 10.376 | 1.00x |
| Text: uncons walk over 100000 characters | 11.454 | 11.511 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 1.240 | 1.236 | 1.00x |
| List: at, 10000 times | 1.422 | 1.428 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 7.892 | 8.353 | 0.94x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.142 | 30.189 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 68.510 | 68.639 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 33.955 | 33.829 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 3.986 | 3.976 | 1.00x |
| Ref.cas loop, 10000 times | 1.048 | 1.052 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.431 | 7.329 | 1.01x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 52.793 | 39.120 | 1.35x |
| Records: a 16-field literal and four getters, 100000 times | 73.433 | 13.481 | 5.45x |
| Counter: 100000 requests | 15.349 | 15.518 | 0.99x |
| Store: get and put, 100000 iterations | 26.285 | 26.477 | 0.99x |
| handle installed per iteration, 10000 iterations | 6.210 | 6.251 | 0.99x |
| Store whose put requests an outer Counter, 100000 iterations | 39.133 | 39.571 | 0.99x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.594 | 1.601 | 1.00x |

- **The same four rows gain, by less**: the match on two arguments 1.38x
  (14.6 ms to 10.6 ms), the Optional built in branches 1.22x, the record
  rows 1.35x and 5.4x. Under the interpreter the saved work is the three
  allocations and tag tests per round against an instruction dispatch
  for everything else, so the ratios are smaller than the JIT's.
- **Everything else within noise**; the Nat.toText row at 0.94x spends
  its time in the builtin, which the rewrites do not touch.

## The library suite

`unison-src/transcripts-manual/jit-suite.md` under `UNISON_JIT=on`, best
of three runs of the before build and two of 7af6ef221 (the rewrites, the
table-free check and the join-point fix below), interleaved.

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 26.603 | 26.614 | 1.00x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 1.00x |
| List.map murmurHash (range 0 1000) | 0.398 | 0.396 | 1.01x |
| Multimap.fromList (range 0 1000) | 0.084 | 0.062 | 1.36x |
| Stream functions | 1.012 | 1.006 | 1.01x |
| Value.serializeUncompressed (10k element map) | 11.119 | 11.103 | 1.00x |
| Value.serializeCompressed (10k element map) | 20.825 | 20.777 | 1.00x |
| Value.deserializeCompressed (10k element map) | 22.092 | 21.806 | 1.01x |
| Json.toText (per document) | 0.007 | 0.007 | 1.00x |
| Json parsing (per document) | 0.010 | 0.010 | 1.01x |
| Json complex parsing (per document) | 0.018 | 0.018 | 1.00x |
| Json complex decoding (per document) | 0.036 | 0.036 | 1.01x |
| Generate 100 random numbers | 0.036 | 0.035 | 1.01x |
| List.foldLeft | 0.166 | 0.166 | 1.00x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.007 | 0.007 | 1.00x |
| CAS an IO.ref 1000 times | 0.019 | 0.019 | 1.00x |
| List.range 0 1000 | 0.051 | 0.050 | 1.03x |
| Set.fromList (range 0 1000) | 0.127 | 0.120 | 1.05x |
| Map.fromList (range 0 1000) | 0.076 | 0.070 | 1.09x |
| NatMap.fromList (range 0 1000) | 0.121 | 0.110 | 1.10x |
| Shuffle a 1000 element array | 0.395 | 0.390 | 1.01x |
| Mutably mergesort a 1000 element array | 0.124 | 0.125 | 1.00x |
| Text.split / | 0.003 | 0.003 | 0.99x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.260 | 0.259 | 1.01x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.052 | 0.052 | 0.99x |
| fib3 | 0.052 | 0.052 | 1.00x |

- **The map builders gain**: Multimap.fromList 1.36x, NatMap.fromList
  1.10x, Map.fromList 1.09x, Set.fromList 1.05x. An insert compares the
  key and matches the result, or builds a pair of the subtrees and takes
  it apart, which the two rules now do in place.
- **Everything else within noise.** No row is slower.

## Found by the suite

Loading the suite's library stopped on `emitClosures: unknown reference`
in a function that builds a pair with lets of its own, matches it, and
inside the arm builds and matches an `Optional`: the outer site's one
tail had its arm copied, renaming the pair's variables to the fields,
and the inner site's join point, made after the copy, took its parameters
from the unoptimized arm through the sources recorded before the split,
so it named the old variable. Fixed in 7af6ef221 (`expand` reads the
renames the split records too), with the shape as `innerUsesOuter` in
jit-tests.md and a check in `prepareBody` that a rewritten body is closed
where the unoptimized one is. The benchmark transcript had no such
nesting, which is why the first rows passed.
