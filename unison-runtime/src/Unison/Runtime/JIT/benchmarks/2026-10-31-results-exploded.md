# 2026-10-31: results exploded, the product-return worker

`unison-src/transcripts-manual/jit-benchmarks.md` and
`unison-src/transcripts-manual/jit-suite.md` on two optimized builds
(`.stack-work-opt`): d4ca6a957 (before, the register entry) and the
product-return worker (design.md "Results exploded"), under
`UNISON_JIT=on`, interleaved, best of three runs of the benchmark
transcript and best of two of the library suite, macOS arm64, the machine
otherwise idle. Times are the transcripts' own, in milliseconds; the ratio
is before over after, so above 1 is a gain. Two versions were measured:
first with the entry rewritten as a call of the worker followed by the
packs, as the design sketch had it, then with the entry keeping its
original body, as a specialisation's does.

## What it showed

- **The row it is for, "State tuple through a non-tail call", 22x** (105 µs
  to 5 µs for 10000 steps): `step acc i = (acc + i, i + 1)` yields its two
  leaves in registers and the loop's match becomes the arm, so the round
  allocates nothing. It had been the one row the wide convention could not
  help (a pair is boxed where it is built, 0.85x at K = 1).
- **With the entry as worker-plus-packs, recursion that embeds its result
  paid twice**: `Tree.insert`'s `Node (insert k v l) k2 v2 r` keeps the
  entry for the recursive call (the result escapes into a field), and that
  entry called the worker, which called the entry: two calls and a repack
  per level. Tree inserts 0.80x, `NatMap.fromList` 0.59x, `Map.fromList`
  0.87x, `Set.fromList` 0.93x, JSON complex decoding 0.94x. With the entry
  keeping its original body all five are back within noise (1.00x, 1.00x,
  1.00x, 0.99x, 1.00x) and the state tuple row keeps its 21.7x.
- **Everything else within noise** in both measurements, the library suite
  included: library code returns products rarely in loops the batch can
  see both sides of.
- **What is left**: a function gets its worker whether or not any site
  takes its result apart (`Tree.insert`'s is dead code), error tails keep a
  function from having one, sums, and the restructuring into explicit ANF
  forms, all in optimization-ideas.md.

## Second measurement: the entry keeps its original body

jit-benchmarks.md:

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.015 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.104 | 0.104 | 1.00x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 1.01x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.00x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.104 | 0.005 | 21.73x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.056 | 0.056 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.036 | 0.035 | 1.02x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.076 | 1.00x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.067 | 0.067 | 1.01x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.510 | 0.510 | 1.00x |
| Text: append "hi" 10000 times | 0.199 | 0.199 | 1.00x |
| Text: drop 1, 100000 times | 1.903 | 1.906 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.198 | 0.199 | 1.00x |
| Bytes: drop 1, 100000 times | 1.845 | 1.845 | 1.00x |
| Bytes: at, 100000 times | 2.096 | 2.097 | 1.00x |
| Text: uncons walk over 100000 characters | 3.288 | 3.288 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.288 | 0.288 | 1.00x |
| List: at, 10000 times | 0.154 | 0.154 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.709 | 0.707 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.001 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.042 | 0.042 | 1.01x |
| Ref.cas loop, 10000 times | 0.131 | 0.131 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.329 | 0.332 | 0.99x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 3.642 | 3.680 | 0.99x |
| Records: a 16-field literal and four getters, 100000 times | 5.047 | 5.041 | 1.00x |
| Counter: 100000 requests | 2.347 | 2.347 | 1.00x |
| Store: get and put, 100000 iterations | 5.506 | 5.539 | 0.99x |
| handle installed per iteration, 10000 iterations | 4.666 | 4.655 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.670 | 8.669 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.472 | 0.472 | 1.00x |

(The "Sum 0 to 1 million" row, which the comparison script misses: 318.5 µs before, 318.7 µs after.)

jit-suite.md:

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 26.798 | 27.226 | 0.98x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 0.99x |
| List.map murmurHash (range 0 1000) | 0.397 | 0.397 | 1.00x |
| Multimap.fromList (range 0 1000) | 0.084 | 0.084 | 1.00x |
| Stream functions | 1.007 | 1.032 | 0.98x |
| Value.serializeUncompressed (10k element map) | 11.055 | 11.222 | 0.99x |
| Value.serializeCompressed (10k element map) | 20.468 | 20.627 | 0.99x |
| Value.deserializeCompressed (10k element map) | 21.786 | 21.774 | 1.00x |
| Json.toText (per document) | 0.007 | 0.007 | 1.00x |
| Json parsing (per document) | 0.010 | 0.010 | 1.00x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.00x |
| Json complex decoding (per document) | 0.036 | 0.035 | 1.00x |
| Generate 100 random numbers | 0.035 | 0.036 | 0.99x |
| List.foldLeft | 0.165 | 0.164 | 1.01x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.007 | 0.007 | 1.02x |
| CAS an IO.ref 1000 times | 0.019 | 0.019 | 1.00x |
| List.range 0 1000 | 0.049 | 0.049 | 1.00x |
| Set.fromList (range 0 1000) | 0.127 | 0.128 | 0.99x |
| Map.fromList (range 0 1000) | 0.076 | 0.076 | 1.00x |
| NatMap.fromList (range 0 1000) | 0.121 | 0.121 | 1.00x |
| Shuffle a 1000 element array | 0.396 | 0.396 | 1.00x |
| Mutably mergesort a 1000 element array | 0.125 | 0.126 | 0.99x |
| Text.split / | 0.003 | 0.003 | 0.99x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.258 | 0.257 | 1.00x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.052 | 0.054 | 0.97x |
| fib3 | 0.052 | 0.052 | 1.00x |

## First measurement: the entry as a call of the worker and the packs

jit-benchmarks.md:

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.015 | 0.98x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.103 | 0.129 | 0.80x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 1.02x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.00x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.105 | 0.005 | 22.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.056 | 0.055 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.036 | 0.036 | 0.99x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.076 | 1.00x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.067 | 0.067 | 1.01x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.510 | 0.510 | 1.00x |
| Text: append "hi" 10000 times | 0.197 | 0.199 | 0.99x |
| Text: drop 1, 100000 times | 1.895 | 1.897 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.197 | 0.198 | 1.00x |
| Bytes: drop 1, 100000 times | 1.815 | 1.840 | 0.99x |
| Bytes: at, 100000 times | 2.098 | 2.096 | 1.00x |
| Text: uncons walk over 100000 characters | 3.288 | 3.281 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.290 | 0.286 | 1.01x |
| List: at, 10000 times | 0.153 | 0.154 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.706 | 0.707 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.042 | 0.042 | 1.00x |
| Ref.cas loop, 10000 times | 0.130 | 0.131 | 0.99x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.330 | 0.331 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 3.643 | 3.649 | 1.00x |
| Records: a 16-field literal and four getters, 100000 times | 5.056 | 5.061 | 1.00x |
| Counter: 100000 requests | 2.305 | 2.357 | 0.98x |
| Store: get and put, 100000 iterations | 5.478 | 5.576 | 0.98x |
| handle installed per iteration, 10000 iterations | 4.662 | 4.675 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.620 | 8.638 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.470 | 0.473 | 0.99x |

(The "Sum 0 to 1 million" row, which the comparison script misses: 318.6 µs before, 318.6 µs after.)

jit-suite.md:

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 26.925 | 27.363 | 0.98x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 1.00x |
| List.map murmurHash (range 0 1000) | 0.395 | 0.394 | 1.00x |
| Multimap.fromList (range 0 1000) | 0.083 | 0.083 | 1.00x |
| Stream functions | 0.999 | 0.998 | 1.00x |
| Value.serializeUncompressed (10k element map) | 11.000 | 10.995 | 1.00x |
| Value.serializeCompressed (10k element map) | 20.380 | 20.417 | 1.00x |
| Value.deserializeCompressed (10k element map) | 21.832 | 21.682 | 1.01x |
| Json.toText (per document) | 0.007 | 0.007 | 1.01x |
| Json parsing (per document) | 0.010 | 0.010 | 1.00x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.00x |
| Json complex decoding (per document) | 0.035 | 0.038 | 0.94x |
| Generate 100 random numbers | 0.035 | 0.035 | 1.00x |
| List.foldLeft | 0.166 | 0.164 | 1.01x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.007 | 0.007 | 1.00x |
| CAS an IO.ref 1000 times | 0.019 | 0.019 | 1.01x |
| List.range 0 1000 | 0.050 | 0.049 | 1.02x |
| Set.fromList (range 0 1000) | 0.128 | 0.137 | 0.93x |
| Map.fromList (range 0 1000) | 0.076 | 0.087 | 0.87x |
| NatMap.fromList (range 0 1000) | 0.121 | 0.206 | 0.59x |
| Shuffle a 1000 element array | 0.395 | 0.397 | 0.99x |
| Mutably mergesort a 1000 element array | 0.124 | 0.124 | 1.00x |
| Text.split / | 0.003 | 0.003 | 1.00x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.257 | 0.255 | 1.01x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.052 | 0.052 | 1.00x |
| fib3 | 0.052 | 0.052 | 1.00x |
