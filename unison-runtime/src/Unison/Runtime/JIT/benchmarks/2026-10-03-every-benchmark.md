# Every benchmark, interpreter against `on` (2026-10-03)

One run each on the optimized build after the Text and Bytes work (M7 step 4), no
statistics. "Speedup" is what to multiply the JIT time by to get the interpreter's. The
`suite` rows at 1.0× are the ones the JIT doesn't touch or that spend their time in Haskell
(serialization, JSON, `Stream`, `Map`). The next full run is
[2026-10-05-every-benchmark-end-of-m7.md](2026-10-05-every-benchmark-end-of-m7.md).

`jitSuite`:

| Benchmark | Interpreter | `jit=on` | Speedup |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 67.4 ms | 316 µs | 213× |
| fib 20 | 1.38 ms | 41.1 µs | 33× |
| Cons list: map with a lambda (1000 elements) | 86 µs | 17.2 µs | 5.01× |
| Cons list: foldLeft with a lambda (1000 elements) | 86.7 µs | 4.89 µs | 18× |
| Binary tree: 1000 inserts | 1.66 ms | 245 µs | 6.76× |
| Binary tree: 1000 lookups | 764 µs | 59.9 µs | 13× |
| Apply a function argument 10000 times | 1.17 ms | 48.7 µs | 24× |
| Mutate a Ref 10000 times | 849 µs | 52.1 µs | 16× |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.78 ms | 68.5 µs | 114× |
| Text: append "hi" 10000 times | 1.08 ms | 317 µs | 3.41× |
| Text: drop 1, 100000 times | 9.56 ms | 2.7 ms | 3.55× |
| Bytes: append 2 bytes 10000 times | 1.04 ms | 322 µs | 3.23× |
| Bytes: drop 1, 100000 times | 8.89 ms | 2.81 ms | 3.17× |
| Bytes: at, 100000 times | 10.2 ms | 2.48 ms | 4.12× |
| Text: uncons walk over 100000 characters | 11 ms | 4.44 ms | 2.47× |
| Nat.toText and Nat.fromText, 10000 times | 8.05 ms | 803 µs | 10× |
| Bytes: decodeNat64be walk over 80000 bytes | 1.49 ms | 595 µs | 2.50× |

`suite`:

| Benchmark | Interpreter | `jit=on` | Speedup |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 113 µs | 10.9 µs | 10× |
| Mutate a local Remote.Ref 10k times | 20.3 ms | 28 ms | 0.72× |
| Do 10k arithmetic operations | 90.2 µs | 1.55 µs | 58× |
| List.map increment (range 0 1000) | 133 µs | 29.6 µs | 4.50× |
| List.map murmurHash (range 0 1000) | 388 µs | 368 µs | 1.05× |
| Multimap.fromList (range 0 1000) | 78.5 µs | 81.1 µs | 0.97× |
| Stream functions | 5.79 ms | 5.9 ms | 0.98× |
| Value.serializeUncompressed (10k element map) | 10.9 ms | 10.8 ms | 1.00× |
| Value.serializeCompressed (10k element map) | 20.3 ms | 20.1 ms | 1.01× |
| Value.deserializeCompressed (10k element map) | 22.2 ms | 22.3 ms | 0.99× |
| Json.toText (per document) | 7.42 µs | 7.46 µs | 1.00× |
| Json parsing (per document) | 10.1 µs | 10.2 µs | 0.99× |
| Json complex parsing (per document) | 17.8 µs | 17.8 µs | 1.00× |
| Json complex decoding (per document) | 63.9 µs | 58.2 µs | 1.10× |
| Decode Nat | 167 ns | 38 ns | 4.39× |
| Generate 100 random numbers | 83.4 µs | 35.8 µs | 2.33× |
| List.foldLeft | 1.07 ms | 202 µs | 5.28× |
| Count to 1 million | 43.3 ms | 320 µs | 135× |
| Count to N (per element) | 83 ns | 4 ns | 21× |
| Count to 1000 | 83.6 µs | 4.88 µs | 17× |
| CAS an IO.ref 1000 times | 127 µs | 115 µs | 1.11× |
| List.range (per element) | 104 ns | 105 ns | 0.99× |
| List.range 0 1000 | 45.6 µs | 45.8 µs | 0.99× |
| Set.fromList (range 0 1000) | 32 µs | 33.6 µs | 0.95× |
| Map.fromList (range 0 1000) | 29.7 µs | 32 µs | 0.93× |
| NatMap.fromList (range 0 1000) | 2.64 ms | 195 µs | 14× |
| Map.lookup (1k element map) | 201 ns | 197 ns | 1.02× |
| Map.insert (1k element map) | 276 ns | 266 ns | 1.04× |
| Shuffle a 1000 element array | 1.21 ms | 543 µs | 2.23× |
| Mutably mergesort a 1000 element array | 3.58 ms | 310 µs | 12× |
| List.at (1k element list) | 123 ns | 24 ns | 5.12× |
| Text.split / | 2.83 µs | 2.77 µs | 1.02× |
| Two match | 5.67 ms | 409 µs | 14× |
| Four match | 5.85 ms | 433 µs | 14× |
| Thirty match | 5.32 ms | 65.1 µs | 82× |
| fib1 | 144 µs | 5.93 µs | 24× |
| fib2 | 337 µs | 336 µs | 1.00× |
| fib3 | 337 µs | 338 µs | 1.00× |
