# 2026-10-29: loop state exploded into parameters

`unison-src/transcripts-manual/jit-benchmarks.md` on the optimized build
(`.stack-work-opt`), the same binary with `UNISON_EXPLODE=off` and on,
interleaved, best of two runs under `UNISON_JIT=on` and best of four under
`UNISON_JIT=off` (the interpreter), macOS arm64. Times are the transcript's
own, in milliseconds; the ratio is off over on, so above 1 is a gain. The
four "Loop state" rows were added for this measurement: a tuple `(acc, i)`
rebuilt each round, a three-field constructor `Pos i line col`, a record
`{ i, line, col }` stepped by `@field.set` updates, and an `Either` whose
constructor alternates each round, 100000 rounds each
(`Unison.Runtime.MCode.Explode`, design.md "Loop state exploded"). The
code measured is the call-pattern specialisation form (one worker per
constructor pattern) with a field read of an exploded record binding no
let; the first form, one worker per function for products only, measured
the same day gave 36x, 8.3x and 71x on the first three rows under the
JIT, and 1.25x, 0.90x and 3.6x under the interpreter.

## The JIT, the explosion off and on

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.328 | 0.329 | 1.00x |
| fib 20 | 0.040 | 0.039 | 1.01x |
| Cons list: map with a lambda (1000 elements) | 0.021 | 0.021 | 1.03x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.03x |
| Binary tree: 1000 inserts | 0.122 | 0.117 | 1.04x |
| Binary tree: 1000 lookups | 0.019 | 0.018 | 1.05x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.015 | 0.015 | 1.00x |
| Cons list: head, 10000 times | 0.024 | 0.023 | 1.06x |
| State tuple through a non-tail call, 10000 steps | 0.118 | 0.109 | 1.09x |
| Ok or Err from a step function, 10000 steps | 0.024 | 0.022 | 1.07x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 1.248 | 0.033 | 38.29x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.574 | 0.055 | 10.39x |
| Loop state: a record updated each round, 100000 rounds | 7.734 | 0.079 | 97.33x |
| Loop state: an Either alternating each round, 100000 rounds | 0.510 | 0.066 | 7.69x |
| Apply a function argument 10000 times | 0.055 | 0.051 | 1.08x |
| Mutate a Ref 10000 times | 0.039 | 0.037 | 1.06x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.083 | 0.080 | 1.04x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.153 | 0.153 | 1.01x |
| Text: append "hi" 10000 times | 0.220 | 0.216 | 1.02x |
| Text: drop 1, 100000 times | 2.246 | 2.097 | 1.07x |
| Bytes: append 2 bytes 10000 times | 0.215 | 0.210 | 1.02x |
| Bytes: drop 1, 100000 times | 2.052 | 2.104 | 0.98x |
| Bytes: at, 100000 times | 2.313 | 2.274 | 1.02x |
| Text: uncons walk over 100000 characters | 3.580 | 3.552 | 1.01x |
| List: cons 10000 elements, then a pattern walk | 0.314 | 0.309 | 1.02x |
| List: at, 10000 times | 0.168 | 0.169 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.762 | 0.770 | 0.99x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.072 | 1.094 | 0.98x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.531 | 0.515 | 1.03x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.568 | 0.570 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.051 | 0.054 | 0.95x |
| Ref.cas loop, 10000 times | 0.135 | 0.134 | 1.01x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.382 | 0.377 | 1.01x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 8.477 | 3.925 | 2.16x |
| Records: a 16-field literal and four getters, 100000 times | 5.669 | 5.470 | 1.04x |
| Counter: 100000 requests | 2.879 | 2.801 | 1.03x |
| Store: get and put, 100000 iterations | 5.902 | 5.456 | 1.08x |
| handle installed per iteration, 10000 iterations | 5.686 | 5.353 | 1.06x |
| Store whose put requests an outer Counter, 100000 iterations | 8.981 | 8.772 | 1.02x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.511 | 0.510 | 1.00x |

## The interpreter, the explosion off and on

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 75.146 | 71.432 | 1.05x |
| fib 20 | 1.454 | 1.399 | 1.04x |
| Cons list: map with a lambda (1000 elements) | 0.111 | 0.098 | 1.13x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.112 | 0.104 | 1.07x |
| Binary tree: 1000 inserts | 1.619 | 1.360 | 1.19x |
| Binary tree: 1000 lookups | 0.746 | 0.677 | 1.10x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.140 | 0.121 | 1.15x |
| Cons list: head, 10000 times | 1.413 | 1.246 | 1.13x |
| State tuple through a non-tail call, 10000 steps | 1.648 | 1.478 | 1.12x |
| Ok or Err from a step function, 10000 steps | 1.878 | 1.625 | 1.16x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 9.766 | 6.897 | 1.42x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 12.131 | 11.258 | 1.08x |
| Loop state: a record updated each round, 100000 rounds | 74.434 | 11.591 | 6.42x |
| Loop state: an Either alternating each round, 100000 rounds | 4.695 | 2.816 | 1.67x |
| Apply a function argument 10000 times | 1.376 | 1.201 | 1.15x |
| Mutate a Ref 10000 times | 0.959 | 0.877 | 1.09x |
| Calls across definitions: Collatz steps for 1 to 1000 | 8.814 | 8.126 | 1.08x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 18.048 | 17.315 | 1.04x |
| Text: append "hi" 10000 times | 0.994 | 0.974 | 1.02x |
| Text: drop 1, 100000 times | 10.468 | 10.005 | 1.05x |
| Bytes: append 2 bytes 10000 times | 0.974 | 1.023 | 0.95x |
| Bytes: drop 1, 100000 times | 9.061 | 9.751 | 0.93x |
| Bytes: at, 100000 times | 11.794 | 11.835 | 1.00x |
| Text: uncons walk over 100000 characters | 12.798 | 12.875 | 0.99x |
| List: cons 10000 elements, then a pattern walk | 1.385 | 1.399 | 0.99x |
| List: at, 10000 times | 1.632 | 1.570 | 1.04x |
| Nat.toText and Nat.fromText, 10000 times | 8.863 | 9.016 | 0.98x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 32.960 | 32.235 | 1.02x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 72.195 | 71.862 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 36.191 | 36.036 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 4.615 | 4.199 | 1.10x |
| Ref.cas loop, 10000 times | 1.242 | 1.209 | 1.03x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 8.480 | 8.387 | 1.01x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 109.877 | 60.160 | 1.83x |
| Records: a 16-field literal and four getters, 100000 times | 82.612 | 87.881 | 0.94x |
| Counter: 100000 requests | 17.525 | 17.486 | 1.00x |
| Store: get and put, 100000 iterations | 30.995 | 29.822 | 1.04x |
| handle installed per iteration, 10000 iterations | 7.313 | 7.124 | 1.03x |
| Store whose put requests an outer Counter, 100000 iterations | 44.604 | 46.326 | 0.96x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.754 | 1.792 | 0.98x |

## What it showed

- **The loops it is for stop allocating.** Under the JIT the tuple loop
  goes 1.25 ms to 0.033 ms (38x), the three-field constructor 0.57 to
  0.055 ms (10x), the record 7.7 to 0.079 ms (97x) and the `Either` 0.51
  to 0.066 ms (7.7x): with the state in parameters each round is a few
  arithmetic instructions and a jump, where before it was an allocation
  (three, for the chained record updates) and a match through memory. The
  `Either` loop runs as two workers, one per constructor, calling each
  other with the field; no tag is ever built or tested.
- **The records row gains 2.2x** without being a loop over a record: the
  functions it calls with record literals, and `bump r = r@x.set (r@x +
  1)` with the record it updates, take the fields where a call in the
  batch builds them, and once a function is specialised per pattern a
  caller that passes a plain variable no longer stops the others.
- **Nothing else moves** under the JIT beyond the run-to-run noise of
  those rows (the plain-call and handler rows at 1.06x to 1.09x are
  within what two runs of them show either way).
- **The interpreter gains too**, since the workers are ordinary
  combinators: the record loop 74 to 11.6 ms (6.4x), the `Either` loop
  1.67x, the tuple loop 1.42x, the records row 1.83x; the three-field
  constructor loop is within noise (1.08x), since under the interpreter
  an allocation costs about what the four-argument worker call does. The
  interpreter's off column ran about 10% slow on rows the change cannot
  touch (tree inserts 1.19x, Ok/Err 1.16x, the MCode of which is
  identical in both modes, checked by dumping it), in all four rounds, so
  read its ratios against that drift; the loop rows are well beyond it.

## Code size, compile time and load time

The test transcript under `UNISON_JIT=on UNISON_JIT_THRESHOLD=1` with the
IR dump, the explosion off and on (the fast build; sizes are of the IR
text handed to LLVM and after its passes):

| | modules | IR text | optimized IR | compile time |
|---|---|---|---|---|
| explode off | 102 | 29.0 MB | 27.1 MB | 17.0 s (generate 6.2, optimize 6.1, codegen 4.7) |
| explode on | 98 | 28.9 MB | 26.9 MB | 16.8 s (generate 6.3, optimize 5.8, codegen 4.7) |

Loading base (a transcript that evaluates `1 + 1`, three runs each): 2.38
to 2.43 s with the explosion off, 2.35 to 2.40 s on. The library suite's
loaded code has about a thousand specialisations among its five thousand
combinators (`UNISON_JIT_DUMP_MCODE=1` over `jit-suite.md`), and no record
instruction at all; the analysis and the extra workers cost nothing
visible at load, and in `on` mode only the hot ones compile.

## The library suite (`jit-suite.md`), the explosion off and on

Ordinary library code (lists, maps, text, JSON, abilities), one run each,
the same optimized binary, `UNISON_EXPLODE=off` and on.

Under the JIT:

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Mutate a Ref 1000 times | 0.014 | 0.014 | 1.00x |
| Mutate a local Remote.Ref 10k times | 31.584 | 32.792 | 0.96x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 0.99x |
| List.map increment (range 0 1000) | 0.024 | 0.023 | 1.01x |
| List.map murmurHash (range 0 1000) | 0.447 | 0.425 | 1.05x |
| Multimap.fromList (range 0 1000) | 0.102 | 0.099 | 1.03x |
| Stream functions | 1.161 | 1.216 | 0.95x |
| Value.serializeUncompressed (10k element map) | 12.947 | 13.092 | 0.99x |
| Value.serializeCompressed (10k element map) | 23.500 | 23.436 | 1.00x |
| Value.deserializeCompressed (10k element map) | 26.245 | 26.506 | 0.99x |
| Json.toText (per document) | 0.008 | 0.008 | 0.97x |
| Json parsing (per document) | 0.011 | 0.012 | 0.93x |
| Json complex parsing (per document) | 0.020 | 0.022 | 0.95x |
| Json complex decoding (per document) | 0.042 | 0.042 | 0.99x |
| Generate 100 random numbers | 0.042 | 0.042 | 0.99x |
| List.foldLeft | 0.190 | 0.186 | 1.03x |
| Count to 1 million | 0.347 | 0.342 | 1.02x |
| Count to 1000 | 0.008 | 0.008 | 1.04x |
| CAS an IO.ref 1000 times | 0.023 | 0.021 | 1.12x |
| List.range 0 1000 | 0.061 | 0.066 | 0.93x |
| Set.fromList (range 0 1000) | 0.160 | 0.156 | 1.03x |
| Map.fromList (range 0 1000) | 0.094 | 0.091 | 1.04x |
| NatMap.fromList (range 0 1000) | 0.148 | 0.149 | 0.99x |
| Shuffle a 1000 element array | 0.471 | 0.471 | 1.00x |
| Mutably mergesort a 1000 element array | 0.146 | 0.144 | 1.01x |
| Text.split / | 0.003 | 0.003 | 0.99x |
| Two match | 0.433 | 0.062 | 7.00x |
| Four match | 0.446 | 0.057 | 7.77x |
| Thirty match | 0.363 | 0.329 | 1.11x |
| fib1 | 0.006 | 0.006 | 1.09x |
| fib2 | 0.064 | 0.065 | 0.97x |
| fib3 | 0.060 | 0.061 | 0.98x |

Under the interpreter:

| row | explode-off (ms) | explode-on (ms) | ratio |
|---|---|---|---|
| Mutate a Ref 1000 times | 0.120 | 0.123 | 0.98x |
| Mutate a local Remote.Ref 10k times | 23.870 | 23.989 | 1.00x |
| Do 10k arithmetic operations | 0.092 | 0.092 | 0.99x |
| List.map increment (range 0 1000) | 0.151 | 0.158 | 0.96x |
| List.map murmurHash (range 0 1000) | 0.442 | 0.437 | 1.01x |
| Multimap.fromList (range 0 1000) | 0.085 | 0.089 | 0.95x |
| Stream functions | 6.657 | 6.692 | 0.99x |
| Value.serializeUncompressed (10k element map) | 12.701 | 12.341 | 1.03x |
| Value.serializeCompressed (10k element map) | 23.956 | 22.711 | 1.05x |
| Value.deserializeCompressed (10k element map) | 29.556 | 25.569 | 1.16x |
| Json.toText (per document) | 0.009 | 0.008 | 1.13x |
| Json parsing (per document) | 0.013 | 0.011 | 1.13x |
| Json complex parsing (per document) | 0.023 | 0.019 | 1.18x |
| Json complex decoding (per document) | 0.083 | 0.070 | 1.18x |
| Generate 100 random numbers | 0.112 | 0.092 | 1.21x |
| List.foldLeft | 1.434 | 1.241 | 1.15x |
| Count to 1 million | 41.819 | 36.493 | 1.15x |
| Count to 1000 | 0.104 | 0.081 | 1.28x |
| CAS an IO.ref 1000 times | 0.161 | 0.136 | 1.19x |
| List.range 0 1000 | 0.065 | 0.054 | 1.21x |
| Set.fromList (range 0 1000) | 0.043 | 0.037 | 1.18x |
| Map.fromList (range 0 1000) | 0.039 | 0.033 | 1.21x |
| NatMap.fromList (range 0 1000) | 3.229 | 2.725 | 1.19x |
| Shuffle a 1000 element array | 1.695 | 1.301 | 1.30x |
| Mutably mergesort a 1000 element array | 5.523 | 3.816 | 1.45x |
| Text.split / | 0.004 | 0.003 | 1.26x |
| Two match | 5.318 | 3.684 | 1.44x |
| Four match | 6.002 | 3.584 | 1.67x |
| Thirty match | 5.285 | 4.217 | 1.25x |
| fib1 | 0.169 | 0.168 | 1.00x |
| fib2 | 0.410 | 0.391 | 1.05x |
| fib3 | 0.391 | 0.386 | 1.01x |

What it showed:

- **Under the JIT the two pattern-matching rows gain 7x and 7.8x** ("Two
  match" 0.433 to 0.062 ms, "Four match" 0.446 to 0.057 ms): loops whose
  state is a tuple of the values matched. Every other row is within the
  noise of one run (0.93x to 1.12x, none consistently).
- **Under the interpreter most rows gain 1.1x to 1.7x**: mergesort 1.45x,
  the two match rows 1.44x and 1.67x, shuffle 1.30x, counting 1.28x, maps
  and sets 1.2x, JSON 1.13x to 1.18x. The rows whose code the explosion
  does not touch (`fib`, arithmetic, a `Ref` mutated) are at 1.00x, so
  this is not drift: about a thousand of the suite's five thousand
  combinators are specialisations, and under the interpreter every pack
  and match they remove was a dispatch each. Native code pays little for
  a bump allocation and a match through memory, which is why the same
  rows barely move under the JIT.

