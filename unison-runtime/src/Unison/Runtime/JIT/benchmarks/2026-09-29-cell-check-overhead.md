# What the native code cell costs the interpreter

2026-09-29, the M0 "overhead" spike: the `GCombInfo` field holding the cell address and the
check in `enter` (read the cell's code pointer, branch on null, bump the counter) were added
with no JIT behind them. Optimized build, `jitSuite`, three runs each; the baseline column is
from before the change. The plan allowed 2%.

| Benchmark | Baseline | With cells | Change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 65.9 to 68.7 ms | 67.5 to 68.6 ms | none measurable |
| fib 20 | 1.33 to 1.37 ms | 1.35 ms | none measurable |
| Cons list: map with a lambda | 81 to 84 µs | 82 to 83 µs | none measurable |
| Cons list: foldLeft with a lambda | 84 to 87 µs | 86 to 88 µs | about +1% |
| Binary tree: 1000 inserts | 1.66 ms | 1.65 to 1.69 ms | none measurable |
| Binary tree: 1000 lookups | 760 to 775 µs | 766 µs | none measurable |
| Apply a function argument 10000 times | 1.13 ms | 1.15 ms | about +1.5% |
| Mutate a Ref 10000 times | 839 to 850 µs | 840 to 859 µs | about +1% |

All within the 2% the plan allowed, so the field and the check stayed. The correctness
transcript still passed, so cells were attached without breaking code loading. (M5 later
found that the *form* of the hot-count test matters: comparing with a configured threshold
and counting in `yield` too made the interpreter 5 to 20% slower; cells now count up from
minus the threshold to zero, so the test is one comparison with a constant.)
