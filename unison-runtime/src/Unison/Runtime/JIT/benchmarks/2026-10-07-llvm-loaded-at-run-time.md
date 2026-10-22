# Every benchmark with LLVM loaded at run time (2026-10-07)

One run each on the optimized build, `--jit off` against `--jit on`, no statistics, after the
switch from linking LLVM to loading it with `dlopen` (commit `366fa6cda`: the JIT is always
built in, LLVM 23.1.2 found in Homebrew's `llvm` keg at startup). Against the
[end-of-M7 run](2026-10-05-every-benchmark-end-of-m7.md) every row is within that run's
noise, which is what was expected: how LLVM's symbols are reached has no bearing on the code
it generates, and the load itself happens once at startup, before anything is measured.

`jitSuite`:

| Benchmark | Interpreter |`jit=on` | Speedup |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 68.2 ms | 319 µs | 214× |
| fib 20 | 1.42 ms | 41 µs | 35× |
| Cons list: map with a lambda (1000 elements) | 90.1 µs | 14.3 µs | 6.32× |
| Cons list: foldLeft with a lambda (1000 elements) | 86.1 µs | 4.86 µs | 18× |
| Binary tree: 1000 inserts | 1.67 ms | 195 µs | 8.57× |
| Binary tree: 1000 lookups | 765 µs | 50.8 µs | 15× |
| Apply a function argument 10000 times | 1.17 ms | 48.5 µs | 24× |
| Mutate a Ref 10000 times | 829 µs | 39.9 µs | 21× |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.77 ms | 69.2 µs | 112× |
| Text: append "hi" 10000 times | 1.07 ms | 261 µs | 4.10× |
| Text: drop 1, 100000 times | 9.49 ms | 2.05 ms | 4.63× |
| Bytes: append 2 bytes 10000 times | 1.03 ms | 251 µs | 4.10× |
| Bytes: drop 1, 100000 times | 8.72 ms | 2.04 ms | 4.28× |
| Bytes: at, 100000 times | 10.1 ms | 2.24 ms | 4.50× |
| Text: uncons walk over 100000 characters | 10.8 ms | 3.59 ms | 3.02× |
| Nat.toText and Nat.fromText, 10000 times | 7.92 ms | 710 µs | 11× |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.5 ms | 992 µs | 31× |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 64.1 ms | 4.26 ms | 15× |
| MutableArray: fill, freeze and sum 10000 elements | 3.91 ms | 45.6 µs | 86× |
| Ref.cas loop, 10000 times | 1.03 ms | 124 µs | 8.30× |
| murmurHashUntyped of Some (i, "x"), 10000 times | 6.76 ms | 263 µs | 26× |
| Bytes: decodeNat64be walk over 80000 bytes | 1.48 ms | 473 µs | 3.14× |

`suite`:

| Benchmark | Interpreter |`jit=on` | Speedup |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 116 µs | 9.47 µs | 12× |
| Mutate a local Remote.Ref 10k times | 20.4 ms | 21.7 ms | 0.94× |
| Do 10k arithmetic operations | 90.8 µs | 1.55 µs | 59× |
| List.map increment (range 0 1000) | 143 µs | 23.1 µs | 6.21× |
| List.map murmurHash (range 0 1000) | 385 µs | 363 µs | 1.06× |
| Multimap.fromList (range 0 1000) | 77.9 µs | 80.3 µs | 0.97× |
| Stream functions | 5.95 ms | 5.91 ms | 1.01× |
| Value.serializeUncompressed (10k element map) | 10.9 ms | 10.9 ms | 1.00× |
| Value.serializeCompressed (10k element map) | 20.1 ms | 20.2 ms | 1.00× |
| Value.deserializeCompressed (10k element map) | 22 ms | 21.8 ms | 1.01× |
| Json.toText (per document) | 7.41 µs | 7.45 µs | 0.99× |
| Json parsing (per document) | 10.2 µs | 10.1 µs | 1.01× |
| Json complex parsing (per document) | 17.9 µs | 17.7 µs | 1.01× |
| Json complex decoding (per document) | 64.5 µs | 57 µs | 1.13× |
| Decode Nat | 170 ns | 31 ns | 5.48× |
| Generate 100 random numbers | 83.5 µs | 35.8 µs | 2.33× |
| List.foldLeft | 1.06 ms | 186 µs | 5.70× |
| Count to 1 million | 43.6 ms | 319 µs | 137× |
| Count to N (per element) | 83 ns | 4 ns | 21× |
| Count to 1000 | 83.3 µs | 4.84 µs | 17× |
| CAS an IO.ref 1000 times | 128 µs | 15.1 µs | 8.46× |
| List.range (per element) | 103 ns | 102 ns | 1.01× |
| List.range 0 1000 | 48.1 µs | 46.2 µs | 1.04× |
| Set.fromList (range 0 1000) | 31.9 µs | 33.2 µs | 0.96× |
| Map.fromList (range 0 1000) | 29.7 µs | 31.8 µs | 0.93× |
| NatMap.fromList (range 0 1000) | 2.68 ms | 152 µs | 18× |
| Map.lookup (1k element map) | 200 ns | 194 ns | 1.03× |
| Map.insert (1k element map) | 278 ns | 267 ns | 1.04× |
| Shuffle a 1000 element array | 1.2 ms | 430 µs | 2.79× |
| Mutably mergesort a 1000 element array | 3.55 ms | 143 µs | 25× |
| List.at (1k element list) | 123 ns | 22 ns | 5.59× |
| Text.split / | 2.84 µs | 2.78 µs | 1.02× |
| Two match | 5.67 ms | 362 µs | 16× |
| Four match | 5.84 ms | 359 µs | 16× |
| Thirty match | 5.31 ms | 66.1 µs | 80× |
| fib1 | 143 µs | 5.96 µs | 24× |
| fib2 | 335 µs | 340 µs | 0.99× |
| fib3 | 336 µs | 336 µs | 1.00× |

The rows at about 1× are, as before, the ones whose time is in Haskell code the JIT doesn't
touch: serialization, JSON, `Map` and `Set`, `Stream`, and the two ability-based `fib`s.
`Remote.Ref` measures where a major GC lands, not the code.
