# Interpreter baseline

2026-09-29, the optimized build of branch `jit` before any JIT code existed, macOS arm64.
These are the numbers the JIT had to beat; time is per run of the benchmark body. `jitSuite`
is the benchmark transcript written for the JIT (`unison-src/transcripts-manual/jit-benchmarks.md`),
`suite` the older, broader one (`jit-suite.md`).

Benchmarks written for the JIT (`jitSuite`):

| Benchmark | Interpreter |
| --- | --- |
| Sum 0 to 1 million | 68.66395ms |
| fib 20 | 1.371103ms |
| Cons list: map with a lambda (1000 elements) | 84.286µs |
| Cons list: foldLeft with a lambda (1000 elements) | 87.023µs |
| Binary tree: 1000 inserts | 1.662015ms |
| Binary tree: 1000 lookups | 760.471µs |
| Apply a function argument 10000 times | 1.129111ms |
| Mutate a Ref 10000 times | 838.72µs |

The existing suite (`suite`), run once by hand for reference. The benchmark transcript doesn't run it:

| Benchmark | Interpreter |
| --- | --- |
| Mutate a local Remote.Ref 10k times | 19.115ms |
| Do 10k arithmetic operations | 88.75µs |
| List.map increment (range 0 1000) | 139.277µs |
| List.map murmurHash (range 0 1000) | 395.484µs |
| Multimap.fromList (range 0 1000) | 65.98µs |
| Stream functions | 5.705434ms |
| Value.serializeUncompressed (10k element map) | 10.614256ms |
| Value.serializeCompressed (10k element map) | 20.456287ms |
| Value.deserializeCompressed (10k element map) | 22.095175ms |
| Json.toText (per document) | 7.158µs |
| Json parsing (per document) | 7.238µs |
| Json complex parsing (per document) | 10.702µs |
| Json complex decoding (per document) | 65.454µs |
| Decode Nat | 172ns |
| Generate 100 random numbers | 82.865µs |
| List.foldLeft | 1.177485ms |
| Count to 1 million | 42.914662ms |
| Count to N (per element) | 80ns |
| Count to 1000 | 80.07µs |
| CAS an IO.ref 1000 times | 128.068µs |
| List.range (per element) | 46ns |
| List.range 0 1000 | 60.964µs |
| Set.fromList (range 0 1000) | 31.458µs |
| Map.fromList (range 0 1000) | 29.114µs |
| NatMap.fromList (range 0 1000) | 2.599734ms |
| Map.lookup (1k element map) | 200ns |
| Map.insert (1k element map) | 272ns |
| Shuffle a 1000 element array | 1.306739ms |
| Mutably mergesort a 1000 element array | 3.578984ms |
| List.at (1k element list) | 143ns |
| Text.split / | 2.908µs |
| Two match | 5.606368ms |
| Four match | 5.912143ms |
| Thirty match | 5.406546ms |
| fib1 | 138.082µs |
| fib2 | 314.239µs |
| fib3 | 311.092µs |
