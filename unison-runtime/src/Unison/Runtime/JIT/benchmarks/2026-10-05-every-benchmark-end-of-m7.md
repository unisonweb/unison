# Every benchmark at the end of M7 (2026-10-05)

One run each on the optimized build, `UNISON_JIT=off` against `on`, no statistics: the
closing tables of M7. Against the
[2026-10-03 run](2026-10-03-every-benchmark.md): the arrays moved (`mergesort` 12× to 23×,
`CAS` 1.1× to 8.1×, `Shuffle` 2.2× to 2.7×), the allocation-heavy rows (the tree, `Ref`, text
and bytes) by 1.1 to 1.5×, and `Remote.Ref` from 0.72× to 0.99×; everything else is within
noise.

`jitSuite`:

| Benchmark | Interpreter |`jit=on` | Speedup |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 65.6 ms | 336 µs | 196× |
| fib 20 | 1.38 ms | 43.1 µs | 32× |
| Cons list: map with a lambda (1000 elements) | 85.7 µs | 14 µs | 6.12× |
| Cons list: foldLeft with a lambda (1000 elements) | 98.1 µs | 5.09 µs | 19× |
| Binary tree: 1000 inserts | 1.67 ms | 215 µs | 7.77× |
| Binary tree: 1000 lookups | 760 µs | 54.2 µs | 14× |
| Apply a function argument 10000 times | 1.16 ms | 49.7 µs | 23× |
| Mutate a Ref 10000 times | 832 µs | 41 µs | 20× |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.72 ms | 68.6 µs | 112× |
| Text: append "hi" 10000 times | 1.07 ms | 263 µs | 4.07× |
| Text: drop 1, 100000 times | 9.58 ms | 2.07 ms | 4.63× |
| Bytes: append 2 bytes 10000 times | 1.04 ms | 252 µs | 4.10× |
| Bytes: drop 1, 100000 times | 8.76 ms | 2.13 ms | 4.12× |
| Bytes: at, 100000 times | 10.4 ms | 2.33 ms | 4.49× |
| Text: uncons walk over 100000 characters | 11 ms | 3.62 ms | 3.04× |
| Nat.toText and Nat.fromText, 10000 times | 8.36 ms | 716 µs | 12× |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 31.5 ms | 1.03 ms | 31× |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 63.5 ms | 4.28 ms | 15× |
| MutableArray: fill, freeze and sum 10000 elements | 4.04 ms | 49.1 µs | 82× |
| Ref.cas loop, 10000 times | 1.04 ms | 126 µs | 8.31× |
| murmurHashUntyped of Some (i, "x"), 10000 times | 6.81 ms | 265 µs | 26× |
| Bytes: decodeNat64be walk over 80000 bytes | 1.75 ms | 484 µs | 3.61× |

`suite`:

| Benchmark | Interpreter |`jit=on` | Speedup |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 113 µs | 9.68 µs | 12× |
| Mutate a local Remote.Ref 10k times | 21.1 ms | 21.4 ms | 0.99× |
| Do 10k arithmetic operations | 91.3 µs | 1.57 µs | 58× |
| List.map increment (range 0 1000) | 134 µs | 23.3 µs | 5.75× |
| List.map murmurHash (range 0 1000) | 385 µs | 362 µs | 1.06× |
| Multimap.fromList (range 0 1000) | 78.3 µs | 81.2 µs | 0.96× |
| Stream functions | 5.84 ms | 5.86 ms | 1.00× |
| Value.serializeUncompressed (10k element map) | 11.1 ms | 10.9 ms | 1.02× |
| Value.serializeCompressed (10k element map) | 20.4 ms | 20.4 ms | 1.00× |
| Value.deserializeCompressed (10k element map) | 22.5 ms | 23.7 ms | 0.95× |
| Json.toText (per document) | 7.47 µs | 7.48 µs | 1.00× |
| Json parsing (per document) | 10.3 µs | 10.1 µs | 1.02× |
| Json complex parsing (per document) | 18 µs | 17.9 µs | 1.00× |
| Json complex decoding (per document) | 64.9 µs | 59.4 µs | 1.09× |
| Decode Nat | 185 ns | 31 ns | 5.97× |
| Generate 100 random numbers | 84.3 µs | 36.1 µs | 2.34× |
| List.foldLeft | 1.07 ms | 186 µs | 5.75× |
| Count to 1 million | 43.6 ms | 320 µs | 136× |
| Count to N (per element) | 84 ns | 4 ns | 21× |
| Count to 1000 | 87.6 µs | 5.06 µs | 17× |
| CAS an IO.ref 1000 times | 128 µs | 15.8 µs | 8.10× |
| List.range (per element) | 106 ns | 106 ns | 1.00× |
| List.range 0 1000 | 48.4 µs | 47.1 µs | 1.03× |
| Set.fromList (range 0 1000) | 32.1 µs | 33.5 µs | 0.96× |
| Map.fromList (range 0 1000) | 29.6 µs | 31.9 µs | 0.93× |
| NatMap.fromList (range 0 1000) | 2.69 ms | 154 µs | 17× |
| Map.lookup (1k element map) | 199 ns | 192 ns | 1.04× |
| Map.insert (1k element map) | 275 ns | 267 ns | 1.03× |
| Shuffle a 1000 element array | 1.22 ms | 457 µs | 2.68× |
| Mutably mergesort a 1000 element array | 3.55 ms | 154 µs | 23× |
| List.at (1k element list) | 125 ns | 22 ns | 5.68× |
| Text.split / | 2.95 µs | 2.81 µs | 1.05× |
| Two match | 5.77 ms | 355 µs | 16× |
| Four match | 5.87 ms | 359 µs | 16× |
| Thirty match | 5.33 ms | 66.6 µs | 80× |
| fib1 | 144 µs | 5.95 µs | 24× |
| fib2 | 338 µs | 340 µs | 0.99× |
| fib3 | 338 µs | 339 µs | 1.00× |

The rows at about 1× are the ones whose time is in Haskell code the JIT doesn't touch:
serialization, JSON, `Map` and `Set`, `Stream`, and the two ability-based `fib`s whose handler
calls exit on every request. `Multimap.fromList` and `Map.fromList` read a few percent slower,
which is within this suite's run-to-run noise. `Remote.Ref` is a single run of 20 ms early in
the suite and measures where a major GC lands, not the code.
