# 2026-10-21: `List.range`, with the result forced

The `List.range` rows of [2026-10-11 replacements](2026-10-11-replacements.md) read 15 to
17 ns at every size, so they never built the list. Measured again with the result forced,
optimized build, one run each, `off` being the Haskell replacement and `on` base's Unison
code compiled (`List_range` is tagged `InterpreterOnly`):

| row | `off` | `on` | on / off |
| --- | --- | --- | --- |
| List.range 0 to 10, forced by size | 273 ns | 156 ns | 0.57 |
| List.range 0 to 1000, forced by size | 14.1 µs | 18.0 µs | 1.28 |
| List.range 0 to 100000, forced by size | 7.67 ms | 1.84 ms | 0.24 |
| List.range 0 to 100000, summed with foldLeft | 17.8 ms | 4.99 ms | 0.28 |

Native is faster at 10 and at 100000 elements and 25% slower at 1000; the tag stands. The
replacement's cost per element grows with the size (14 ns at 1000, 77 ns at 100000), the
native one's doesn't (18 ns at both).
