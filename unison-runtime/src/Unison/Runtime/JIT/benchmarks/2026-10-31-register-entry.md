# 2026-10-31: the register entry, calls between modules in registers

`unison-src/transcripts-manual/jit-benchmarks.md` and
`unison-src/transcripts-manual/jit-suite.md` on two optimized builds
(`.stack-work-opt`), the K = 1 binary of 775cda44b (before) and
d4ca6a957 (after, the register entry), under `UNISON_JIT=on`, interleaved,
best of three runs of the benchmark transcript and best of two of the
library suite, macOS arm64, the machine otherwise idle. Times are the
transcripts' own, in milliseconds; the ratio is before over after, so
above 1 is a gain for the register entry. Both binaries at K = 1, so the
only difference is the second word in the cell and the calls that use it.

## What it showed

- **Every row within noise, including the two "Calls across definitions"
  rows** (1.00x and 0.99x). The claim in design.md and in the ideas file
  that those rows pay the uniform entry was wrong: the compile log and the
  IR dump show `collatzStep`, `collatzSteps` and `collatzTotal` in one
  module, and `oneInThree` taken along into `branchCalls`'s batch (as the
  comment above it in the transcript already says), so every hot call in
  them is direct. The register path is taken in those rows only by the
  timing thunk, once per repeat, and inside the `printTime` harness.
- **So two rows were added that do cross modules** ("callee native before
  the caller"): `warmSteps` makes `stepsAlone` native in a batch of its own
  before `totalAlone` and `manyAlone` get hot, and a looping callee that
  already has code isn't copied into its caller's batch (`copyUnits` copies
  only non-looping ones), so the call goes through the cell. Verified on the
  IR dump: the caller's module calls `%fx` (the entry) with `call tailcc`.
  Three alternating runs of a transcript holding just these rows, the
  "before" binary against the "after" one, and the "after" binary with
  `UNISON_JIT_DISABLE=xcall` (same code, entry never taken), best of
  three (two for the disabled run), in microseconds:

  | row | before | after, entry off | after | before / after |
  |---|---|---|---|---|
  | Collatz steps for 1 to 1000 (one module, the old row) | 76.9 | 75.9 | 75.8 | 1.01x |
  | callee native before the caller, Collatz steps for 1 to 1000 | 69.2 | 71.0 | 66.7 | 1.04x |
  | callee native before the caller, 100000 calls of a one-step callee | 707.7 | 780.7 | 509.6 | 1.39x |

  The one-step row is the mechanism itself: about 7.1 ns per iteration
  through the uniform entry (store the argument to the Unison stack, the
  wrapper loads it, calls the worker, stores the result, the caller reloads
  it, around a C-convention call) against 5.1 ns through the register entry,
  a 2 ns saving per cross-module call. The Collatz-shaped row amortizes
  those 2 ns over a hundred loop iterations per call, hence 4%. The
  "entry off" column is a little slower than the before binary on the
  one-step row (781 against 708 µs) with the same call path; a layout
  difference between the binaries, not measured further.
- **The library suite is unchanged** (within noise; `Multimap.fromList`
  1.12x and `fib2` 0.94x are the usual spread of those rows). Library code
  under `UNISON_JIT=on` crosses modules at the rate its batches split, and
  nothing in the suite is a tight loop of cross-module calls.
- The two new rows are in the benchmark transcript from here on, so a
  change to the entry or to batching shows up.

## jit-benchmarks.md

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.015 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.103 | 0.104 | 0.99x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 1.03x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.00x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.104 | 0.104 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.056 | 0.056 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.036 | 0.035 | 1.03x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.075 | 0.076 | 0.99x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Text: append "hi" 10000 times | 0.209 | 0.207 | 1.01x |
| Text: drop 1, 100000 times | 1.931 | 1.914 | 1.01x |
| Bytes: append 2 bytes 10000 times | 0.201 | 0.199 | 1.01x |
| Bytes: drop 1, 100000 times | 1.869 | 1.850 | 1.01x |
| Bytes: at, 100000 times | 2.109 | 2.094 | 1.01x |
| Text: uncons walk over 100000 characters | 3.297 | 3.294 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.293 | 0.290 | 1.01x |
| List: at, 10000 times | 0.153 | 0.154 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.701 | 0.707 | 0.99x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.044 | 0.042 | 1.04x |
| Ref.cas loop, 10000 times | 0.129 | 0.130 | 0.99x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.321 | 0.329 | 0.98x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 3.657 | 3.634 | 1.01x |
| Records: a 16-field literal and four getters, 100000 times | 5.046 | 5.039 | 1.00x |
| Counter: 100000 requests | 2.288 | 2.329 | 0.98x |
| Store: get and put, 100000 iterations | 5.165 | 5.495 | 0.94x |
| handle installed per iteration, 10000 iterations | 4.648 | 4.681 | 0.99x |
| Store whose put requests an outer Counter, 100000 iterations | 8.394 | 8.661 | 0.97x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.460 | 0.474 | 0.97x |

(The "Sum 0 to 1 million" row, which the comparison script misses: 318.6 µs before, 318.6 µs after.)

## jit-suite.md

| row | before (ms) | after (ms) | before / after |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 27.095 | 26.892 | 1.01x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 0.99x |
| List.map murmurHash (range 0 1000) | 0.397 | 0.393 | 1.01x |
| Multimap.fromList (range 0 1000) | 0.093 | 0.083 | 1.12x |
| Stream functions | 1.005 | 1.007 | 1.00x |
| Value.serializeUncompressed (10k element map) | 11.001 | 11.004 | 1.00x |
| Value.serializeCompressed (10k element map) | 20.422 | 20.444 | 1.00x |
| Value.deserializeCompressed (10k element map) | 21.663 | 22.012 | 0.98x |
| Json.toText (per document) | 0.007 | 0.007 | 1.00x |
| Json parsing (per document) | 0.010 | 0.010 | 1.00x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.00x |
| Json complex decoding (per document) | 0.035 | 0.036 | 0.99x |
| Generate 100 random numbers | 0.035 | 0.035 | 1.00x |
| List.foldLeft | 0.164 | 0.165 | 1.00x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.006 | 0.007 | 0.99x |
| CAS an IO.ref 1000 times | 0.019 | 0.019 | 0.98x |
| List.range 0 1000 | 0.049 | 0.050 | 1.00x |
| Set.fromList (range 0 1000) | 0.128 | 0.126 | 1.01x |
| Map.fromList (range 0 1000) | 0.077 | 0.076 | 1.01x |
| NatMap.fromList (range 0 1000) | 0.117 | 0.121 | 0.97x |
| Shuffle a 1000 element array | 0.399 | 0.395 | 1.01x |
| Mutably mergesort a 1000 element array | 0.127 | 0.124 | 1.02x |
| Text.split / | 0.003 | 0.003 | 1.02x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.256 | 0.257 | 1.00x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.050 | 0.053 | 0.94x |
| fib3 | 0.050 | 0.052 | 0.96x |
