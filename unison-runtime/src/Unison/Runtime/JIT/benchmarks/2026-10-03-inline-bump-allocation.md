# Inline bump allocation

Native code and the C helpers bump a copy of the current allocation block's free pointer
instead of calling `allocate` for every object (2026-10-03), and the allocation budget was
then folded into the bump limit (2026-10-04). `jitSuite` on the optimized build, one run each
(the two bump runs agreed to within 1% on every row, the better is shown). Rows that don't
allocate are unchanged, as they should be.

| Benchmark | Interpreter | `on` before | `on` with bump allocation | Change |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 67.4 ms | 316 µs | 314 µs | 1.01× |
| fib 20 | 1.38 ms | 41.1 µs | 41 µs | 1.00× |
| Cons list: map with a lambda (1000 elements) | 86 µs | 17.2 µs | 14.3 µs | 1.20× |
| Cons list: foldLeft with a lambda (1000 elements) | 86.7 µs | 4.89 µs | 4.86 µs | 1.01× |
| Binary tree: 1000 inserts | 1.66 ms | 245 µs | 197 µs | 1.25× |
| Binary tree: 1000 lookups | 764 µs | 59.9 µs | 49.8 µs | 1.20× |
| Apply a function argument 10000 times | 1.17 ms | 48.7 µs | 48.5 µs | 1.01× |
| Mutate a Ref 10000 times | 849 µs | 52.1 µs | 34.1 µs | 1.53× |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.78 ms | 68.5 µs | 67.3 µs | 1.02× |
| Text: append "hi" 10000 times | 1.08 ms | 317 µs | 269 µs | 1.18× |
| Text: drop 1, 100000 times | 9.56 ms | 2.7 ms | 2.07 ms | 1.30× |
| Bytes: append 2 bytes 10000 times | 1.04 ms | 322 µs | 268 µs | 1.20× |
| Bytes: drop 1, 100000 times | 8.89 ms | 2.81 ms | 2.18 ms | 1.29× |
| Bytes: at, 100000 times | 10.2 ms | 2.48 ms | 2.25 ms | 1.10× |
| Text: uncons walk over 100000 characters | 11 ms | 4.44 ms | 3.7 ms | 1.20× |
| Nat.toText and Nat.fromText, 10000 times | 8.05 ms | 803 µs | 723 µs | 1.11× |
| Bytes: decodeNat64be walk over 80000 bytes | 1.49 ms | 595 µs | 491 µs | 1.21× |

With the budget folded into the limit (2026-10-04), against the counter version above,
best of two runs each. A few percent on the rows that allocate most, nothing anywhere else;
kept for the simpler fast path.

| Benchmark | Interpreter | bump, budget counter | bump, budget folded | Change |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 67.4 ms | 314 µs | 315 µs | 1.00× |
| fib 20 | 1.38 ms | 41 µs | 41 µs | 1.00× |
| Cons list: map with a lambda (1000 elements) | 86 µs | 14.3 µs | 13.9 µs | 1.03× |
| Cons list: foldLeft with a lambda (1000 elements) | 86.7 µs | 4.86 µs | 4.87 µs | 1.00× |
| Binary tree: 1000 inserts | 1.66 ms | 197 µs | 195 µs | 1.01× |
| Binary tree: 1000 lookups | 764 µs | 49.8 µs | 49.5 µs | 1.01× |
| Apply a function argument 10000 times | 1.17 ms | 48.5 µs | 48.5 µs | 1.00× |
| Mutate a Ref 10000 times | 849 µs | 34.1 µs | 33.8 µs | 1.01× |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.78 ms | 67.3 µs | 67.4 µs | 1.00× |
| Text: append "hi" 10000 times | 1.08 ms | 269 µs | 255 µs | 1.06× |
| Text: drop 1, 100000 times | 9.56 ms | 2.07 ms | 2.04 ms | 1.01× |
| Bytes: append 2 bytes 10000 times | 1.04 ms | 268 µs | 253 µs | 1.06× |
| Bytes: drop 1, 100000 times | 8.89 ms | 2.18 ms | 2.05 ms | 1.06× |
| Bytes: at, 100000 times | 10.2 ms | 2.25 ms | 2.26 ms | 1.00× |
| Text: uncons walk over 100000 characters | 11 ms | 3.7 ms | 3.57 ms | 1.03× |
| Nat.toText and Nat.fromText, 10000 times | 8.05 ms | 723 µs | 717 µs | 1.01× |
| Bytes: decodeNat64be walk over 80000 bytes | 1.49 ms | 491 µs | 487 µs | 1.01× |

The broad `suite` with `on` after the first step (the counter version), same binaries, one
run each: the allocation-heavy rows moved and nothing else did (every other row within 5%,
both ways, which is this suite's noise).

| Benchmark | `on` before | `on` with bump allocation | Change |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 10.9 µs | 9.18 µs | 1.19× |
| List.map increment (range 0 1000) | 29.6 µs | 24.3 µs | 1.22× |
| Decode Nat | 38 ns | 32 ns | 1.19× |
| List.foldLeft | 202 µs | 188 µs | 1.07× |
| NatMap.fromList (range 0 1000) | 195 µs | 153 µs | 1.27× |
| Mutably mergesort a 1000 element array | 310 µs | 256 µs | 1.21× |
| List.at (1k element list) | 24 ns | 22 ns | 1.09× |
| Two match | 409 µs | 310 µs | 1.32× |
| Four match | 433 µs | 308 µs | 1.40× |
