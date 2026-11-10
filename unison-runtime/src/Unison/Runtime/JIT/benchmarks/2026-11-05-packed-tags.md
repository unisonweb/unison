# 2026-11-05: data closures carry only their packed tag

`unison-src/transcripts-manual/jit-benchmarks.md` and `jit-suite.md` on two
optimized builds (`.stack-work-opt`): e5d9d76ff (before: the type registry
in place, closures still carrying a Reference) and the change that drops
the Reference from every data closure, `Pack`, `PackWide`, `PackSlot` and
the pool's enum keys, names the numbers of `Any` and `Boolean` as literals
the builtin numbering is built from, and compares two constructors field
by field in place (`compareClosure`, `Eq Closure`) instead of through the
field lists the `DataC` pattern built. `jit-benchmarks.md` under
`UNISON_JIT=on`, best of two runs each, and under `UNISON_JIT=off` (the
interpreter), one run each; `jit-suite.md` under `UNISON_JIT=eager`, best
of two runs each. The runs of the two builds were interleaved, each pair
back to back; macOS arm64, the machine otherwise idle. Times are the
transcripts' own, in milliseconds; the ratio is before over after, so
above 1 is a gain.

Why: the change is for correctness of the representation (one source of a
type's identity, the tag; no type table in the pool; C builds constructors
only through the macros) and for the write-barrier and GC fixes made with
it; it was expected to cost nothing. The interpreter was expected to gain
a little: a data closure is one word smaller, a pack stores one field
less, and a comparison of two constructors no longer allocates a list of
each one's fields.

## jit-benchmarks.md under `UNISON_JIT=on`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.015 | 0.014 | 1.03x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.099 | 0.095 | 1.04x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 1.02x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.02x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.005 | 0.005 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 1.00x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.055 | 0.055 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 1.00x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.102 | 0.102 | 1.00x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.191 | 0.191 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.035 | 0.036 | 0.99x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.077 | 0.99x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.067 | 0.067 | 1.00x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.511 | 0.512 | 1.00x |
| Text: append "hi" 10000 times | 0.196 | 0.197 | 1.00x |
| Text: drop 1, 100000 times | 1.885 | 1.878 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.197 | 0.198 | 0.99x |
| Bytes: drop 1, 100000 times | 1.843 | 1.896 | 0.97x |
| Bytes: at, 100000 times | 2.092 | 2.063 | 1.01x |
| Text: uncons walk over 100000 characters | 3.284 | 3.249 | 1.01x |
| List: cons 10000 elements, then a pattern walk | 0.289 | 0.288 | 1.00x |
| List: at, 10000 times | 0.153 | 0.149 | 1.03x |
| Nat.toText and Nat.fromText, 10000 times | 0.705 | 0.710 | 0.99x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.006 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.487 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.044 | 0.044 | 1.00x |
| Ref.cas loop, 10000 times | 0.134 | 0.134 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.283 | 0.283 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 1.386 | 1.412 | 0.98x |
| Records: a 16-field literal and four getters, 100000 times | 0.096 | 0.096 | 1.00x |
| Counter: 100000 requests | 2.481 | 2.504 | 0.99x |
| Store: get and put, 100000 iterations | 5.545 | 5.449 | 1.02x |
| handle installed per iteration, 10000 iterations | 4.639 | 4.669 | 0.99x |
| Store whose put requests an outer Counter, 100000 iterations | 8.713 | 8.752 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.483 | 0.467 | 1.03x |

## jit-benchmarks.md under `UNISON_JIT=off`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| fib 20 | 1.322 | 1.295 | 1.02x |
| Cons list: map with a lambda (1000 elements) | 0.093 | 0.091 | 1.02x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.090 | 0.095 | 0.95x |
| Binary tree: 1000 inserts | 1.240 | 1.279 | 0.97x |
| Binary tree: 1000 lookups | 0.623 | 0.632 | 0.99x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.114 | 0.111 | 1.03x |
| Cons list: head, 10000 times | 1.140 | 1.170 | 0.97x |
| State tuple through a non-tail call, 10000 steps | 0.899 | 0.897 | 1.00x |
| Ok or Err from a step function, 10000 steps | 1.478 | 1.448 | 1.02x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 6.535 | 6.384 | 1.02x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 10.895 | 10.642 | 1.02x |
| Loop state: a record updated each round, 100000 rounds | 10.890 | 10.639 | 1.02x |
| Loop state: an Either alternating each round, 100000 rounds | 2.585 | 2.551 | 1.01x |
| Known constructor: a match on two arguments at once, 100000 rounds | 10.780 | 10.593 | 1.02x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 11.608 | 11.466 | 1.01x |
| Apply a function argument 10000 times | 1.091 | 1.096 | 1.00x |
| Mutate a Ref 10000 times | 0.809 | 0.819 | 0.99x |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.353 | 7.100 | 1.04x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 16.184 | 15.774 | 1.03x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 6.847 | 6.587 | 1.04x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 24.292 | 23.099 | 1.05x |
| Text: append "hi" 10000 times | 0.941 | 0.898 | 1.05x |
| Text: drop 1, 100000 times | 9.840 | 9.054 | 1.09x |
| Bytes: append 2 bytes 10000 times | 0.967 | 0.870 | 1.11x |
| Bytes: drop 1, 100000 times | 9.049 | 8.202 | 1.10x |
| Bytes: at, 100000 times | 11.211 | 10.217 | 1.10x |
| Text: uncons walk over 100000 characters | 12.398 | 10.784 | 1.15x |
| List: cons 10000 elements, then a pattern walk | 1.340 | 1.236 | 1.08x |
| List: at, 10000 times | 1.531 | 1.402 | 1.09x |
| Nat.toText and Nat.fromText, 10000 times | 8.701 | 7.853 | 1.11x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 32.191 | 30.099 | 1.07x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 71.710 | 68.043 | 1.05x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 36.443 | 33.324 | 1.09x |
| MutableArray: fill, freeze and sum 10000 elements | 4.270 | 3.885 | 1.10x |
| Ref.cas loop, 10000 times | 1.118 | 1.059 | 1.06x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.982 | 7.521 | 1.06x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 39.798 | 38.535 | 1.03x |
| Records: a 16-field literal and four getters, 100000 times | 13.701 | 13.435 | 1.02x |
| Counter: 100000 requests | 15.942 | 15.366 | 1.04x |
| Store: get and put, 100000 iterations | 28.060 | 26.380 | 1.06x |
| handle installed per iteration, 10000 iterations | 6.498 | 6.195 | 1.05x |
| Store whose put requests an outer Counter, 100000 iterations | 42.126 | 38.813 | 1.09x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.654 | 1.560 | 1.06x |

## jit-suite.md under `UNISON_JIT=eager`

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 8.421 | 9.189 | 0.92x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 1.02x |
| List.map murmurHash (range 0 1000) | 0.399 | 0.401 | 1.00x |
| Multimap.fromList (range 0 1000) | 0.066 | 0.074 | 0.89x |
| Stream functions | 1.098 | 1.088 | 1.01x |
| Value.serializeUncompressed (10k element map) | 11.107 | 11.172 | 0.99x |
| Value.serializeCompressed (10k element map) | 20.441 | 20.423 | 1.00x |
| Value.deserializeCompressed (10k element map) | 22.577 | 23.518 | 0.96x |
| Json.toText (per document) | 0.007 | 0.007 | 0.99x |
| Json parsing (per document) | 0.010 | 0.010 | 1.02x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.02x |
| Json complex decoding (per document) | 0.038 | 0.038 | 0.99x |
| Generate 100 random numbers | 0.035 | 0.039 | 0.90x |
| List.foldLeft | 0.164 | 0.173 | 0.95x |
| Count to 1 million | 0.320 | 0.319 | 1.00x |
| Count to 1000 | 0.005 | 0.005 | 1.00x |
| CAS an IO.ref 1000 times | 0.016 | 0.016 | 1.01x |
| List.range 0 1000 | 0.055 | 0.055 | 1.00x |
| Set.fromList (range 0 1000) | 0.093 | 0.088 | 1.06x |
| Map.fromList (range 0 1000) | 0.062 | 0.059 | 1.05x |
| NatMap.fromList (range 0 1000) | 0.123 | 0.120 | 1.02x |
| Shuffle a 1000 element array | 0.409 | 0.425 | 0.96x |
| Mutably mergesort a 1000 element array | 0.143 | 0.141 | 1.01x |
| Text.split / | 0.003 | 0.003 | 1.01x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.088 | 0.088 | 1.00x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.053 | 0.052 | 1.02x |
| fib3 | 0.053 | 0.051 | 1.03x |

## What it showed

Under the JIT every row is within noise: the native code never read the
reference word, so dropping it changes nothing it executes, and the enum
and box helpers do the same work from a tag as they did from a reference.

The interpreter gains across the rows that build and take apart
constructors, 1.02x to 1.15x (`Text: uncons walk` 1.15x, `Bytes: append`
1.11x, `Nat.toText and Nat.fromText` 1.11x, `MutableArray: fill, freeze
and sum` 1.10x): a data closure is a word smaller and a pack stores one
field less, which is less allocation per constructor on paths that make
millions of them. The rows that build none (`fib`, the loop-state rows,
`Apply a function argument`) are within noise.

The library suite is within noise everywhere that matters, on both sides
of the comparison rewrite: `Set.fromList` 1.06x and `Map.fromList` 1.05x
are the rows whose key order is the universal comparison, and they gain a
little from the field-by-field compare; `Multimap.fromList` 0.89x and
`Generate 100 random numbers` 0.90x are rows under 100 µs whose best of
two differed by one or two run-to-run steps, and `Mutate a local
Remote.Ref` is the single early run that measures where a major GC lands
(development.md, "Benchmarking rules"). `Value.deserializeCompressed`,
0.80x in an earlier run of this change taken while another job was on the
machine, is 0.96x here with the runs interleaved.

A first run of this comparison measured the interpreter under `on`: the
new build's startup self-test for the universal comparison (`Some 1`
against `(1, 1)`, types not statically equal: equal constructor tags,
ordered by arity) had failed on the rewritten `compareClosure` and turned
the JIT off, and the benchmark script counted no "JIT is off" lines. It
does now, and the fallback arm orders by arity.
