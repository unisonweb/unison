# Arrays and Refs

Every array builtin and the `Ref`/`Ticket` ones became C helpers on 2026-10-05. Optimized
build, interpreter against `UNISON_JIT=on`. The three new `jitSuite` rows name the builtins
through base's `Raw` namespaces (`mutable.ByteArray.Raw.read64le` and so on).

| Benchmark | Interpreter | `jit=on` | Speedup |
| --- | --- | --- | --- |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 63.6 ms | 4.34 ms | 15× |
| MutableArray: fill, freeze and sum 10000 elements | 3.85 ms | 45.7 µs | 84× |
| Ref.cas loop, 10000 times | 1.03 ms | 124 µs | 8.3× |

The broad `suite` with `on`, the rows that use arrays and refs (before = the previous
commit, same binary otherwise):

| Benchmark | Interpreter | `on` before | `on` now | Change |
| --- | --- | --- | --- | --- |
| Mutably mergesort a 1000 element array | 314 µs | 306 µs | 137 µs | 2.2× |
| Shuffle a 1000 element array | 797 µs | 790 µs | 430 µs | 1.8× |
| CAS an IO.ref 1000 times | 174 µs | 173 µs | 14.9 µs | 11.6× |
| Generate 100 random numbers | 58.1 µs | 58.1 µs | 36.3 µs | 1.6× |
| List.map murmurHash (range 0 1000) | 421 µs | 443 µs | 363 µs | 1.2× |

The murmurHash row, flagged as open after the private-copies change, is back below its
earlier numbers in this run; the earlier reading looks like noise of that row, and the entry
is closed. Every other row is within noise.
