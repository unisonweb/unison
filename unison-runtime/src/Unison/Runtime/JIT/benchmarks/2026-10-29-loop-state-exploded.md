# 2026-10-29: loop state exploded into parameters

`unison-src/transcripts-manual/jit-benchmarks.md` on the optimized build
(`.stack-work-opt`), the same binary with `UNISON_EXPLODE=off` and on,
interleaved, best of two runs under `UNISON_JIT=on` and best of two under
`UNISON_JIT=off` (the interpreter), macOS arm64. Times are the transcript's
own, in milliseconds; the ratio is off over on, so above 1 is a gain. The
three "Loop state" rows were added for this measurement: a tuple `(acc, i)`
rebuilt each round, a three-field constructor `Pos i line col`, and a record
`{ i, line, col }` stepped by `@field.set` updates, 100000 rounds each
(`Unison.Runtime.MCode.Explode`, design.md "Loop state exploded").

## The JIT, the explosion off and on

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.329 | 0.327 | 1.01x |
| fib 20 | 0.040 | 0.040 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.021 | 0.021 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 0.99x |
| Binary tree: 1000 inserts | 0.112 | 0.113 | 0.99x |
| Binary tree: 1000 lookups | 0.016 | 0.014 | 1.08x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.015 | 0.015 | 0.97x |
| Cons list: head, 10000 times | 0.023 | 0.023 | 0.99x |
| State tuple through a non-tail call, 10000 steps | 0.108 | 0.111 | 0.97x |
| Ok or Err from a step function, 10000 steps | 0.023 | 0.023 | 0.98x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 1.184 | 0.033 | 36.32x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.462 | 0.056 | 8.28x |
| Loop state: a record updated each round, 100000 rounds | 7.033 | 0.099 | 70.97x |
| Apply a function argument 10000 times | 0.051 | 0.052 | 0.98x |
| Mutate a Ref 10000 times | 0.036 | 0.036 | 1.01x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.083 | 0.080 | 1.03x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.156 | 0.152 | 1.03x |
| Text: append "hi" 10000 times | 0.211 | 0.216 | 0.98x |
| Text: drop 1, 100000 times | 2.094 | 2.085 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.218 | 0.213 | 1.02x |
| Bytes: drop 1, 100000 times | 2.025 | 2.015 | 1.00x |
| Bytes: at, 100000 times | 2.284 | 2.273 | 1.00x |
| Text: uncons walk over 100000 characters | 3.588 | 3.568 | 1.01x |
| List: cons 10000 elements, then a pattern walk | 0.303 | 0.306 | 0.99x |
| List: at, 10000 times | 0.165 | 0.169 | 0.98x |
| Nat.toText and Nat.fromText, 10000 times | 0.751 | 0.735 | 1.02x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.122 | 1.080 | 1.04x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.516 | 0.503 | 1.02x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.571 | 0.577 | 0.99x |
| MutableArray: fill, freeze and sum 10000 elements | 0.052 | 0.051 | 1.01x |
| Ref.cas loop, 10000 times | 0.131 | 0.134 | 0.98x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.373 | 0.383 | 0.97x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 7.757 | 6.128 | 1.27x |
| Records: a 16-field literal and four getters, 100000 times | 5.391 | 5.413 | 1.00x |
| Counter: 100000 requests | 2.812 | 2.733 | 1.03x |
| Store: get and put, 100000 iterations | 5.325 | 5.540 | 0.96x |
| handle installed per iteration, 10000 iterations | 5.414 | 5.286 | 1.02x |
| Store whose put requests an outer Counter, 100000 iterations | 8.673 | 8.782 | 0.99x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.494 | 0.492 | 1.01x |

## The interpreter, the explosion off and on

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 65.749 | 70.177 | 0.94x |
| fib 20 | 1.294 | 1.387 | 0.93x |
| Cons list: map with a lambda (1000 elements) | 0.096 | 0.101 | 0.94x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.095 | 0.099 | 0.96x |
| Binary tree: 1000 inserts | 1.306 | 1.377 | 0.95x |
| Binary tree: 1000 lookups | 0.659 | 0.675 | 0.98x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.118 | 0.120 | 0.98x |
| Cons list: head, 10000 times | 1.222 | 1.254 | 0.97x |
| State tuple through a non-tail call, 10000 steps | 1.394 | 1.467 | 0.95x |
| Ok or Err from a step function, 10000 steps | 1.566 | 1.579 | 0.99x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 8.506 | 6.827 | 1.25x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 10.014 | 11.165 | 0.90x |
| Loop state: a record updated each round, 100000 rounds | 62.566 | 17.168 | 3.64x |
| Apply a function argument 10000 times | 1.194 | 1.165 | 1.03x |
| Mutate a Ref 10000 times | 0.860 | 0.910 | 0.95x |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.516 | 7.672 | 0.98x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 17.458 | 16.697 | 1.05x |
| Text: append "hi" 10000 times | 0.947 | 0.953 | 0.99x |
| Text: drop 1, 100000 times | 9.922 | 9.999 | 0.99x |
| Bytes: append 2 bytes 10000 times | 0.965 | 0.898 | 1.07x |
| Bytes: drop 1, 100000 times | 9.209 | 9.236 | 1.00x |
| Bytes: at, 100000 times | 11.092 | 11.282 | 0.98x |
| Text: uncons walk over 100000 characters | 3.269 | 12.533 | 0.26x |
| List: cons 10000 elements, then a pattern walk | 1.372 | 1.340 | 1.02x |
| List: at, 10000 times | 1.527 | 1.525 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 8.591 | 8.616 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 31.652 | 31.651 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 71.009 | 70.209 | 1.01x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 35.814 | 35.584 | 1.01x |
| MutableArray: fill, freeze and sum 10000 elements | 4.121 | 3.985 | 1.03x |
| Ref.cas loop, 10000 times | 1.169 | 1.117 | 1.05x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.857 | 8.149 | 0.96x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 105.010 | 95.178 | 1.10x |
| Records: a 16-field literal and four getters, 100000 times | 78.841 | 79.007 | 1.00x |
| Counter: 100000 requests | 16.752 | 16.366 | 1.02x |
| Store: get and put, 100000 iterations | 29.310 | 28.517 | 1.03x |
| handle installed per iteration, 10000 iterations | 6.850 | 6.772 | 1.01x |
| Store whose put requests an outer Counter, 100000 iterations | 42.675 | 42.366 | 1.01x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.662 | 1.738 | 0.96x |

## What it showed

- **The loops it is for stop allocating.** Under the JIT the tuple loop
  goes 1.18 ms to 0.033 ms (36x), the three-field constructor 0.46 to
  0.056 ms (8.3x) and the record 7.0 to 0.099 ms (71x): with the state in
  parameters each round is a few arithmetic instructions and a jump, where
  before it was an allocation (or three, for the chained record updates)
  and a match through memory. The record row's gain is the largest because
  a record update was the most expensive step: three `RecSet`s per round,
  each copying the record.
- **The records row gains 1.27x** without being a loop over a record:
  `bump r = r@x.set (r@x + 1)` and its callers take their record apart
  where a call in the batch builds it.
- **Nothing else moves** under the JIT: every other row is within 4%,
  and the two rows just outside (Binary tree lookups 1.08x, Store 0.96x)
  are the usual noise of those rows.
- **The interpreter gains on the record loop (3.6x) and the tuple loop
  (1.25x)** and loses 10% on the three-field constructor loop: its
  worker call copies four arguments (`VArgN`) where the loop before
  packed three fields and matched them, and under the interpreter an
  allocation is cheap next to its dispatch, so the two are close; the
  record's three `RecSet` copies per round are what made that loop slow.
  The interpreter's "Text: uncons walk" at 0.26x is one outlier run (3.27
  ms in the first off run; the second off run, both on runs, and a
  standalone timing of that row in either mode are all 12.5 ms), and the
  row has no product state for the rewrite to touch.
- A comparison against yesterday's commit (`wc1*` runs) showed every row
  7% slower, the float and FFI rows included, which the explosion cannot
  touch: the machine was not in the same state across the two days. The
  interleaved off/on runs above are the measurement.
