# 2026-11-05: LLVM's shrink wrapping off

The cost of `-enable-shrink-wrap=false`, which jit_llvm.c passes to LLVM since fc1ffbe4b (the
AArch64 backend let a frameless return of a `tailcc` worker skip the pop of its stack arguments;
development.md has the crash). One optimized build (`.stack-work-opt` at fc1ffbe4b), the two arms on
the same binary: `UNISON_JIT_LLVM_OPTS=-enable-shrink-wrap=true` under `UNISON_JIT_DEBUG=1` turns
shrink wrapping back on after the built-in option, the other arm the same with no extra option.
`jit-benchmarks.md` under `UNISON_JIT=on` and `jit-suite.md` under `UNISON_JIT=eager`, interleaved,
best of two runs each, times in milliseconds; the ratio is shrink wrapping on over off, so above 1
means the flag gained. A macOS media-indexing daemon held a core throughout, so the noise is wider
than usual; the interleaving puts it on both arms.

Shrink wrapping only spares the frame setup on a function's frameless paths, which in generated
code are the early returns of workers (a loop's exit test at entry) and the fast paths of the
wrappers; the loops run with the frame either way. Within noise on every row of both suites.

## `jit-benchmarks.md`, `UNISON_JIT=on`

| row | shrink wrapping (ms) | off (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.313 | 0.319 | 0.98x |
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.014 | 0.015 | 0.97x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.093 | 0.095 | 0.98x |
| Binary tree: 1000 lookups | 0.014 | 0.014 | 1.00x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.01x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.005 | 0.005 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 0.99x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.055 | 0.055 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 0.99x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.101 | 0.102 | 0.99x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.188 | 0.188 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.035 | 0.036 | 0.97x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.077 | 0.075 | 1.02x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.066 | 0.066 | 1.00x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.494 | 0.494 | 1.00x |
| Text: append "hi" 10000 times | 0.195 | 0.195 | 1.00x |
| Text: drop 1, 100000 times | 1.879 | 1.849 | 1.02x |
| Bytes: append 2 bytes 10000 times | 0.195 | 0.195 | 1.00x |
| Bytes: drop 1, 100000 times | 1.827 | 1.834 | 1.00x |
| Bytes: at, 100000 times | 2.065 | 2.066 | 1.00x |
| Text: uncons walk over 100000 characters | 3.258 | 3.260 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.284 | 0.283 | 1.00x |
| List: at, 10000 times | 0.149 | 0.149 | 1.00x |
| Nat.toText and Nat.fromText, 10000 times | 0.710 | 0.709 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.042 | 0.043 | 0.99x |
| Ref.cas loop, 10000 times | 0.129 | 0.131 | 0.99x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.275 | 0.276 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 1.367 | 1.390 | 0.98x |
| Records: a 16-field literal and four getters, 100000 times | 0.096 | 0.096 | 1.00x |
| Counter: 100000 requests | 2.380 | 2.367 | 1.01x |
| Store: get and put, 100000 iterations | 5.205 | 5.331 | 0.98x |
| handle installed per iteration, 10000 iterations | 4.681 | 4.684 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.464 | 8.599 | 0.98x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.446 | 0.459 | 0.97x |

## `jit-suite.md`, `UNISON_JIT=eager`

| row | shrink wrapping (ms) | off (ms) | ratio |
|---|---|---|---|
| Mutate a Ref 1000 times | 0.009 | 0.009 | 1.00x |
| Mutate a local Remote.Ref 10k times | 8.239 | 8.145 | 1.01x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 1.00x |
| List.map increment (range 0 1000) | 0.020 | 0.020 | 1.00x |
| List.map murmurHash (range 0 1000) | 0.390 | 0.393 | 0.99x |
| Multimap.fromList (range 0 1000) | 0.071 | 0.071 | 1.00x |
| Stream functions | 1.074 | 1.077 | 1.00x |
| Value.serializeUncompressed (10k element map) | 11.115 | 11.120 | 1.00x |
| Value.serializeCompressed (10k element map) | 20.332 | 20.299 | 1.00x |
| Value.deserializeCompressed (10k element map) | 22.641 | 22.687 | 1.00x |
| Json.toText (per document) | 0.007 | 0.007 | 1.00x |
| Json parsing (per document) | 0.010 | 0.010 | 1.00x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.00x |
| Json complex decoding (per document) | 0.037 | 0.037 | 0.99x |
| Generate 100 random numbers | 0.036 | 0.037 | 1.00x |
| List.foldLeft | 0.161 | 0.161 | 1.00x |
| Count to 1 million | 0.319 | 0.319 | 1.00x |
| Count to 1000 | 0.005 | 0.005 | 1.00x |
| CAS an IO.ref 1000 times | 0.015 | 0.015 | 1.00x |
| List.range 0 1000 | 0.052 | 0.052 | 1.00x |
| Set.fromList (range 0 1000) | 0.086 | 0.087 | 1.00x |
| Map.fromList (range 0 1000) | 0.058 | 0.058 | 1.00x |
| NatMap.fromList (range 0 1000) | 0.116 | 0.117 | 0.99x |
| Shuffle a 1000 element array | 0.406 | 0.411 | 0.99x |
| Mutably mergesort a 1000 element array | 0.141 | 0.141 | 1.00x |
| Text.split / | 0.003 | 0.003 | 1.00x |
| Two match | 0.053 | 0.053 | 1.00x |
| Four match | 0.053 | 0.053 | 1.00x |
| Thirty match | 0.088 | 0.088 | 1.00x |
| fib1 | 0.005 | 0.005 | 1.00x |
| fib2 | 0.051 | 0.051 | 1.00x |
| fib3 | 0.051 | 0.051 | 1.00x |
