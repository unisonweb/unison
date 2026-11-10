# 2026-11-04: no product-return worker for a function whose recursion embeds its result

`unison-src/transcripts-manual/jit-benchmarks.md` and `jit-suite.md` on two
optimized builds (`.stack-work-opt`): e9d6e4701 (before) and the two changes
to the product-return worker in `Unison.Runtime.MCode.Explode` (design.md
"Results exploded"): a function that lets the result of a call of itself
and uses it whole gets no worker (`selfResultEscapes`), and a caller that
uses a result whole keeps the call of the entry (the use count in
`useReturns` was taken after `rebuild`, which rebinds the variable, so it
was always zero and every call of a product-returning function was
rewritten and followed by a repack). Under `UNISON_JIT=on`, best of two
runs each, and under `UNISON_JIT=off` (the interpreter), best of two
before and one run after; `jit-suite.md` under `on`, one run each; macOS
arm64, the machine otherwise idle. Times are the transcripts' own, in
milliseconds; the ratio is before over after, so above 1 is a gain. The
"before" runs are those of 2026-11-03-wide-anf-pass.md.

Why: `Binary tree: 1000 inserts` was 0.76x against trunk under the JIT and
0.73x in the interpreter. The MCode dump showed `Tree.insert`'s entry
calling its own worker and packing the four leaves back into a `Node` at
every level, the shape 2026-10-31-results-exploded.md had found and
thought removed: the worker's recursion yielded the leaves and the entry
repacked them, so each level paid a four-value yield and a `Pack` more.
With the row alone under each switch, `-explode_returns` recovered the
whole interpreter loss and most of the JIT loss; `-wide` was worth 5%.

## jit-benchmarks.md under `UNISON_JIT=on`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.015 | 1.01x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.117 | 0.103 | 1.13x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 0.97x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.00x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.005 | 0.005 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.055 | 0.056 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.102 | 0.102 | 1.00x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.190 | 0.188 | 1.01x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.036 | 0.036 | 0.98x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.076 | 1.00x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.067 | 0.067 | 1.00x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.510 | 0.510 | 1.00x |
| Text: append "hi" 10000 times | 0.199 | 0.200 | 1.00x |
| Text: drop 1, 100000 times | 1.910 | 1.904 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.200 | 0.198 | 1.01x |
| Bytes: drop 1, 100000 times | 1.839 | 1.840 | 1.00x |
| Bytes: at, 100000 times | 2.099 | 2.103 | 1.00x |
| Text: uncons walk over 100000 characters | 3.299 | 3.294 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.292 | 0.289 | 1.01x |
| List: at, 10000 times | 0.153 | 0.154 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.707 | 0.706 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.043 | 0.042 | 1.02x |
| Ref.cas loop, 10000 times | 0.133 | 0.133 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.336 | 0.345 | 0.97x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 1.388 | 1.395 | 1.00x |
| Records: a 16-field literal and four getters, 100000 times | 0.096 | 0.096 | 1.00x |
| Counter: 100000 requests | 2.435 | 2.372 | 1.03x |
| Store: get and put, 100000 iterations | 5.494 | 5.761 | 0.95x |
| handle installed per iteration, 10000 iterations | 4.679 | 4.667 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.718 | 8.946 | 0.97x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.486 | 0.484 | 1.00x |

## jit-benchmarks.md under `UNISON_JIT=off`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| fib 20 | 1.269 | 1.260 | 1.01x |
| Cons list: map with a lambda (1000 elements) | 0.088 | 0.089 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.089 | 0.088 | 1.00x |
| Binary tree: 1000 inserts | 1.607 | 1.235 | 1.30x |
| Binary tree: 1000 lookups | 0.620 | 0.619 | 1.00x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.111 | 0.111 | 1.00x |
| Cons list: head, 10000 times | 1.124 | 1.125 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.905 | 0.898 | 1.01x |
| Ok or Err from a step function, 10000 steps | 1.460 | 1.456 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 6.367 | 6.372 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 10.653 | 10.657 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 10.667 | 10.658 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 2.529 | 2.539 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 10.583 | 10.588 | 1.00x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 11.498 | 11.486 | 1.00x |
| Apply a function argument 10000 times | 1.096 | 1.094 | 1.00x |
| Mutate a Ref 10000 times | 0.801 | 0.801 | 1.00x |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.127 | 7.106 | 1.00x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 15.782 | 15.766 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 6.586 | 6.575 | 1.00x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 23.268 | 23.314 | 1.00x |
| Text: append "hi" 10000 times | 0.896 | 0.897 | 1.00x |
| Text: drop 1, 100000 times | 9.037 | 9.041 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.870 | 0.872 | 1.00x |
| Bytes: drop 1, 100000 times | 8.191 | 8.235 | 0.99x |
| Bytes: at, 100000 times | 10.434 | 10.365 | 1.01x |
| Text: uncons walk over 100000 characters | 11.425 | 11.537 | 0.99x |
| List: cons 10000 elements, then a pattern walk | 1.244 | 1.246 | 1.00x |
| List: at, 10000 times | 1.424 | 1.417 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 7.948 | 8.070 | 0.98x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.174 | 30.112 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 68.799 | 68.865 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 33.857 | 34.164 | 0.99x |
| MutableArray: fill, freeze and sum 10000 elements | 3.944 | 4.031 | 0.98x |
| Ref.cas loop, 10000 times | 1.056 | 1.056 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.334 | 7.295 | 1.01x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 38.093 | 38.189 | 1.00x |
| Records: a 16-field literal and four getters, 100000 times | 13.426 | 13.408 | 1.00x |
| Counter: 100000 requests | 15.358 | 15.338 | 1.00x |
| Store: get and put, 100000 iterations | 26.307 | 26.319 | 1.00x |
| handle installed per iteration, 10000 iterations | 6.225 | 6.223 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 39.077 | 39.112 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.595 | 1.631 | 0.98x |

## jit-suite.md under `UNISON_JIT=on`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 27.172 | 27.126 | 1.00x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.01x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 1.01x |
| List.map murmurHash (range 0 1000) | 0.400 | 0.401 | 1.00x |
| Multimap.fromList (range 0 1000) | 0.062 | 0.062 | 1.00x |
| Stream functions | 1.002 | 1.006 | 1.00x |
| Value.serializeUncompressed (10k element map) | 11.072 | 11.286 | 0.98x |
| Value.serializeCompressed (10k element map) | 20.624 | 21.264 | 0.97x |
| Value.deserializeCompressed (10k element map) | 21.873 | 22.246 | 0.98x |
| Json.toText (per document) | 0.007 | 0.007 | 0.99x |
| Json parsing (per document) | 0.010 | 0.010 | 0.99x |
| Json complex parsing (per document) | 0.017 | 0.018 | 0.99x |
| Json complex decoding (per document) | 0.036 | 0.036 | 0.99x |
| Generate 100 random numbers | 0.037 | 0.036 | 1.04x |
| List.foldLeft | 0.167 | 0.167 | 1.00x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.007 | 0.006 | 1.07x |
| CAS an IO.ref 1000 times | 0.020 | 0.019 | 1.06x |
| List.range 0 1000 | 0.051 | 0.052 | 0.98x |
| Set.fromList (range 0 1000) | 0.123 | 0.123 | 1.00x |
| Map.fromList (range 0 1000) | 0.073 | 0.070 | 1.05x |
| NatMap.fromList (range 0 1000) | 0.110 | 0.111 | 0.99x |
| Shuffle a 1000 element array | 0.392 | 0.388 | 1.01x |
| Mutably mergesort a 1000 element array | 0.126 | 0.123 | 1.02x |
| Text.split / | 0.003 | 0.003 | 0.98x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.259 | 0.258 | 1.00x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.052 | 0.053 | 0.99x |
| fib3 | 0.052 | 0.053 | 1.00x |

## What it showed

- **Tree inserts 1.13x under the JIT and 1.30x in the interpreter**
  (0.117 to 0.103 ms, 1.61 to 1.24 ms): `Tree.insert` keeps its entry
  only, and its group's MCode is now the same as under
  `-explode_returns`. Against trunk the row is 0.86x under the JIT and
  0.95x in the interpreter, from 0.76x and 0.73x. Of what remains under
  the JIT, about 5% is the wide convention's `Box` of the recursive
  result before it goes into the `Node`; the rest is not understood (the
  row at `UNISON_OPTIMIZATIONS=none` on the branch matches trunk).
- **Every other row within noise** in both suites, the state tuple row
  (the worker's own benchmark) at its 15x against trunk: the worker is
  still made for a function whose self calls are taken apart, or that has
  none.
- **The library suite's `Set.fromList` (0.81x), `NatMap.fromList`
  (0.85x) and `Thirty match` (0.37x) against trunk are unchanged** by
  this, each from one run; they are the branch's, not the worker's, and
  are the rows to look at next.
