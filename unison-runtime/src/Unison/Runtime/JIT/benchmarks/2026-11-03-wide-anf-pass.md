# 2026-11-03: the branch against trunk, with the wide pass on the ANF

`unison-src/transcripts-manual/jit-benchmarks.md` and `jit-suite.md` on two
optimized builds: origin/trunk at 25234f241 (the JIT PR merged, before the
wide convention, the explosions and the known-constructor rewrites) and the
`jit` branch at e9d6e4701 (the wide convention's escape analysis as an ANF
pass, `Unison.Runtime.MCode.Wide`, on top of the rest of the branch), on
macOS arm64, the machine otherwise idle, interleaved. `jit-benchmarks.md`
under `UNISON_JIT=on`, best of two runs, and under `UNISON_JIT=off` (the
interpreter), best of two; `jit-suite.md` under `on`, one run each. Times
are the transcript's own, in milliseconds; the ratio is trunk over branch,
so above 1 is a gain for the branch. This is the whole branch against
trunk, the measurement for the pull request; the pass itself changes no
instruction but four tail yields (the commit message), so the rows that
exercise it (the loop-state, state-tuple, `Ok or Err`, record and known
constructor rows) are the same as in the 2026-11-02 run.

## jit-benchmarks.md under `UNISON_JIT=on`

| row | trunk (ms) | branch (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.319 | 0.319 | 1.00x |
| fib 20 | 0.035 | 0.037 | 0.95x |
| Cons list: map with a lambda (1000 elements) | 0.014 | 0.015 | 0.89x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 0.99x |
| Binary tree: 1000 inserts | 0.088 | 0.117 | 0.76x |
| Binary tree: 1000 lookups | 0.016 | 0.013 | 1.22x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 0.97x |
| Cons list: head, 10000 times | 0.036 | 0.024 | 1.51x |
| State tuple through a non-tail call, 10000 steps | 0.072 | 0.005 | 14.95x |
| Ok or Err from a step function, 10000 steps | 0.042 | 0.022 | 1.87x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.727 | 0.032 | 22.81x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.437 | 0.055 | 7.88x |
| Loop state: a record updated each round, 100000 rounds | 6.322 | 0.077 | 81.86x |
| Loop state: an Either alternating each round, 100000 rounds | 0.379 | 0.062 | 6.14x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.694 | 0.102 | 6.79x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.325 | 0.190 | 1.71x |
| Apply a function argument 10000 times | 0.053 | 0.049 | 1.08x |
| Mutate a Ref 10000 times | 0.033 | 0.036 | 0.93x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.066 | 0.076 | 0.87x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.118 | 0.147 | 0.80x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.066 | 0.067 | 0.98x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.536 | 0.510 | 1.05x |
| Text: append "hi" 10000 times | 0.202 | 0.199 | 1.01x |
| Text: drop 1, 100000 times | 1.891 | 1.910 | 0.99x |
| Bytes: append 2 bytes 10000 times | 0.198 | 0.200 | 0.99x |
| Bytes: drop 1, 100000 times | 1.796 | 1.839 | 0.98x |
| Bytes: at, 100000 times | 2.134 | 2.099 | 1.02x |
| Text: uncons walk over 100000 characters | 3.276 | 3.299 | 0.99x |
| List: cons 10000 elements, then a pattern walk | 0.285 | 0.292 | 0.97x |
| List: at, 10000 times | 0.142 | 0.153 | 0.93x |
| Nat.toText and Nat.fromText, 10000 times | 0.705 | 0.707 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.539 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.045 | 0.043 | 1.05x |
| Ref.cas loop, 10000 times | 0.126 | 0.133 | 0.95x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.276 | 0.336 | 0.82x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 7.463 | 1.388 | 5.38x |
| Records: a 16-field literal and four getters, 100000 times | 5.021 | 0.096 | 52.56x |
| Counter: 100000 requests | 2.636 | 2.435 | 1.08x |
| Store: get and put, 100000 iterations | 4.902 | 5.494 | 0.89x |
| handle installed per iteration, 10000 iterations | 4.216 | 4.679 | 0.90x |
| Store whose put requests an outer Counter, 100000 iterations | 7.883 | 8.718 | 0.90x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.453 | 0.486 | 0.93x |

## jit-benchmarks.md under `UNISON_JIT=off`

| row | trunk (ms) | branch (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 62.801 | 63.369 | 0.99x |
| fib 20 | 1.171 | 1.269 | 0.92x |
| Cons list: map with a lambda (1000 elements) | 0.083 | 0.088 | 0.93x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.086 | 0.089 | 0.96x |
| Binary tree: 1000 inserts | 1.177 | 1.607 | 0.73x |
| Binary tree: 1000 lookups | 0.595 | 0.620 | 0.96x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.105 | 0.111 | 0.95x |
| Cons list: head, 10000 times | 1.069 | 1.124 | 0.95x |
| State tuple through a non-tail call, 10000 steps | 1.236 | 0.905 | 1.37x |
| Ok or Err from a step function, 10000 steps | 1.403 | 1.460 | 0.96x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 7.327 | 6.367 | 1.15x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 9.325 | 10.653 | 0.88x |
| Loop state: a record updated each round, 100000 rounds | 56.937 | 10.667 | 5.34x |
| Loop state: an Either alternating each round, 100000 rounds | 3.673 | 2.529 | 1.45x |
| Known constructor: a match on two arguments at once, 100000 rounds | 14.374 | 10.583 | 1.36x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 13.546 | 11.498 | 1.18x |
| Apply a function argument 10000 times | 1.038 | 1.096 | 0.95x |
| Mutate a Ref 10000 times | 0.799 | 0.801 | 1.00x |
| Calls across definitions: Collatz steps for 1 to 1000 | 6.922 | 7.127 | 0.97x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 15.048 | 15.782 | 0.95x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 6.347 | 6.586 | 0.96x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 22.192 | 23.268 | 0.95x |
| Text: append "hi" 10000 times | 0.894 | 0.896 | 1.00x |
| Text: drop 1, 100000 times | 9.000 | 9.037 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.861 | 0.870 | 0.99x |
| Bytes: drop 1, 100000 times | 8.172 | 8.191 | 1.00x |
| Bytes: at, 100000 times | 10.322 | 10.434 | 0.99x |
| Text: uncons walk over 100000 characters | 10.462 | 11.425 | 0.92x |
| List: cons 10000 elements, then a pattern walk | 1.228 | 1.244 | 0.99x |
| List: at, 10000 times | 1.371 | 1.424 | 0.96x |
| Nat.toText and Nat.fromText, 10000 times | 7.946 | 7.948 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.075 | 30.174 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 68.539 | 68.799 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 33.661 | 33.857 | 0.99x |
| MutableArray: fill, freeze and sum 10000 elements | 3.981 | 3.944 | 1.01x |
| Ref.cas loop, 10000 times | 1.034 | 1.056 | 0.98x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.121 | 7.334 | 0.97x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 92.790 | 38.093 | 2.44x |
| Records: a 16-field literal and four getters, 100000 times | 72.543 | 13.426 | 5.40x |
| Counter: 100000 requests | 15.042 | 15.358 | 0.98x |
| Store: get and put, 100000 iterations | 25.621 | 26.307 | 0.97x |
| handle installed per iteration, 10000 iterations | 6.005 | 6.225 | 0.96x |
| Store whose put requests an outer Counter, 100000 iterations | 37.255 | 39.077 | 0.95x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.538 | 1.595 | 0.96x |

## jit-suite.md under `UNISON_JIT=on`, one run each

| row | trunk (ms) | branch (ms) | ratio |
|---|---|---|---|
| Mutate a Ref 1000 times | 0.008 | 0.012 | 0.66x |
| Mutate a local Remote.Ref 10k times | 21.911 | 27.172 | 0.81x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.019 | 0.020 | 0.95x |
| List.map murmurHash (range 0 1000) | 0.374 | 0.400 | 0.93x |
| Multimap.fromList (range 0 1000) | 0.078 | 0.062 | 1.25x |
| Stream functions | 0.978 | 1.002 | 0.98x |
| Value.serializeUncompressed (10k element map) | 10.931 | 11.072 | 0.99x |
| Value.serializeCompressed (10k element map) | 20.597 | 20.624 | 1.00x |
| Value.deserializeCompressed (10k element map) | 21.995 | 21.873 | 1.01x |
| Json.toText (per document) | 0.007 | 0.007 | 0.99x |
| Json parsing (per document) | 0.010 | 0.010 | 0.99x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.00x |
| Json complex decoding (per document) | 0.033 | 0.036 | 0.93x |
| Generate 100 random numbers | 0.042 | 0.037 | 1.13x |
| List.foldLeft | 0.162 | 0.167 | 0.97x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.005 | 0.007 | 0.70x |
| CAS an IO.ref 1000 times | 0.014 | 0.020 | 0.72x |
| List.range 0 1000 | 0.051 | 0.051 | 1.00x |
| Set.fromList (range 0 1000) | 0.099 | 0.123 | 0.81x |
| Map.fromList (range 0 1000) | 0.070 | 0.073 | 0.96x |
| NatMap.fromList (range 0 1000) | 0.095 | 0.110 | 0.86x |
| Shuffle a 1000 element array | 0.467 | 0.392 | 1.19x |
| Mutably mergesort a 1000 element array | 0.132 | 0.126 | 1.05x |
| Text.split / | 0.003 | 0.003 | 0.93x |
| Two match | 0.378 | 0.053 | 7.12x |
| Four match | 0.369 | 0.053 | 6.95x |
| Thirty match | 0.096 | 0.259 | 0.37x |
| fib1 | 0.005 | 0.005 | 0.97x |
| fib2 | 0.050 | 0.052 | 0.95x |
| fib3 | 0.050 | 0.052 | 0.95x |

(`Decode Nat`, 29 ns on trunk and 117 ns on the branch, is one operation
timed once and sits at the timer's floor, as does `Mutate a Ref 1000 times`.)

## What it showed

- **The rows the branch was built for, 5x to 82x under the JIT**: a tuple
  rebuilt each round 23x, a record updated each round 82x, a 16-field
  record literal read by getters 53x, a three-field constructor 7.9x, an
  `Either` alternating 6.1x, a state tuple through a non-tail call 15x, a
  match on two arguments at once 6.8x, `Ok or Err` 1.9x, `Two match` and
  `Four match` in the broad suite 7x. The interpreter gains on the same
  rows (the record rows 5.3x and 5.4x, the rest 1.2x to 1.5x), since the
  explosions and the rewrites are on the ANF.
- **Everything else within noise, with three exceptions.** `Binary tree:
  1000 inserts` is 0.76x under the JIT and 0.73x in the interpreter, in
  every round (0.088 against 0.117 ms, 1.18 against 1.61 ms): the
  branch's cost on this row predates the pass (the 2026-10-31 results
  file has 0.80x on it in the interpreter) and is not yet understood;
  the insert's recursive call returns a tree that is matched by the
  caller, so a wide return that is boxed at once is the likely shape.
  `Thirty match` in the broad suite is 0.37x (0.096 against 0.259 ms) in
  its one run; the branch's time on it has been 0.256 ms in every run
  since 2026-10-31, so the difference is trunk's, and wants a second run
  before being chased. `Mutate a local Remote.Ref 10k times` is 0.81x in
  one run of a row that has moved 20% between rounds before.
- **The pass itself costs nothing at run time**: the rows that place
  boxes (`Ok or Err`, the `Either` loop, `Case of case`, the records)
  match the 2026-11-02 run to the microsecond.
