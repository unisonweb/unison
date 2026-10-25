# Native lists in the `suite`

Two runs of the broad `suite` on 2026-10-02, optimized build, one run each. The first is
M6's steps 5, 5b and 7 (lists on the first strict structure, with native list, text and
partial-application helpers); the second is the swap to the finger tree (`Deque2` at the
time, `Unison.Util.Deque` now) with every list primitive native. "`off` before" is the
interpreter on `Data.Sequence`.

## Native list, text and partial-application helpers (M6 steps 5, 5b, 7)

| Benchmark | `off` before | `off` | `on` | `on` / `off` |
| --- | --- | --- | --- | --- |
| List.map increment | 145 µs | 151 µs | 64.8 µs | 0.43 |
| List.map murmurHash | 396 µs | 402 µs | 412 µs | 1.02 |
| List.foldLeft | 1.20 ms | 1.16 ms | 343 µs | 0.30 |
| List.at | 146 ns | 130 ns | 25 ns | 0.20 |
| List.range 0 1000 | 58.1 µs | 45.1 µs | 44.7 µs | 0.99 |
| List.range (per element) | 42 ns | 102 ns | 103 ns | 1.01 |
| Multimap.fromList | 63.9 µs | 82.9 µs | 85.4 µs | 1.03 |
| Set.fromList, Map.fromList | 31.4, 29.0 µs | 27.4, 25.8 µs | 28.5, 27.5 µs | 1.04, 1.07 |
| Json.toText | 7.05 µs | 8.43 µs | 8.42 µs | 1.00 |
| Json parsing, complex parsing | 7.19, 10.7 µs | 10.2, 18.1 µs | 10.2, 18.2 µs | 1.00, 1.00 |
| Json complex decoding | 62.1 µs | 64.3 µs | 59.3 µs | 0.92 |
| Generate 100 random numbers | 83.5 µs | 84.5 µs | 39.4 µs | 0.47 |
| Shuffle a 1000 element array | 1.32 ms | 1.27 ms | 636 µs | 0.50 |
| Text.split / | 2.87 µs | 3.26 µs | 3.29 µs | 1.01 |
| Stream functions | 5.80 ms | 5.72 ms | 6.08 ms | 1.06 |
| Mutate a local Remote.Ref (one run of 20 ms) | 19.7 ms | 19.7 ms | 26.3 ms | 1.34 |

Everything not listed is as at step 8. The `on` column is from before step 7 (partial
applications), which the suite barely exercises. The interpreter's own regressions from
the swap are the "`off` before" against "`off`" columns: JSON parsing, `Multimap.fromList`
and `List.range` per element (see the ideas, "The list structure itself").

`jitSuite` (`off`, then `on`): 65.7 ms / 319 µs, 1.39 ms / 41.2 µs, 86.6 µs / 16.7 µs,
86.9 µs / 4.85 µs, 1.67 ms / 203 µs, 762 µs / 56.9 µs, 1.16 ms / 48.3 µs, 828 µs / 50.8 µs,
7.73 ms / 68.0 µs, text append 1.48 ms / 548 µs (was 1.08 ms), text drop 11.7 ms / 3.54 ms
(was 9.19 ms).
## Lists on the finger tree, every list primitive native

| Benchmark | `off` Deque | `off` Deque2 | `on` Deque | `on` Deque2 | `on` / `off` |
| --- | --- | --- | --- | --- | --- |
| List.map increment | 151 µs | 130 µs | 65.1 µs | 29.7 µs | 0.23 |
| List.map murmurHash | 402 µs | 382 µs | 414 µs | 364 µs | 0.95 |
| List.foldLeft | 1.16 ms | 1.06 ms | 345 µs | 204 µs | 0.19 |
| List.at | 130 ns | 123 ns | 25 ns | 23 ns | 0.19 |
| List.range 0 1000 | 45.1 µs | 45.8 µs | 45.1 µs | 46.8 µs | 1.02 |
| List.range (per element) | 102 ns | 103 ns | 102 ns | 104 ns | 1.01 |
| Multimap.fromList | 82.9 µs | 76.9 µs | 84.9 µs | 80.6 µs | 1.05 |
| Set.fromList, Map.fromList | 27.4, 25.8 µs | 31.9, 29.4 µs | 28.6, 27.6 µs | 33.9, 33.1 µs | 1.06, 1.13 |
| Json.toText | 8.43 µs | 6.11 µs | 8.45 µs | 6.20 µs | 1.01 |
| Json parsing, complex parsing | 10.2, 18.1 µs | 10.1, 18.1 µs | 10.2, 18.2 µs | 10.4, 18.5 µs | 1.03, 1.02 |
| Json complex decoding | 64.3 µs | 63.0 µs | 58.2 µs | 57.9 µs | 0.92 |
| Generate 100 random numbers | 84.5 µs | 83.4 µs | 40.0 µs | 36.1 µs | 0.43 |
| Shuffle a 1000 element array | 1.27 ms | 1.20 ms | 643 µs | 540 µs | 0.45 |
| Mutably mergesort a 1000 element array | 3.56 ms | 3.52 ms | 335 µs | 293 µs | 0.08 |
| Mutate a local Remote.Ref (one run of 20 ms) | 19.7 ms | 19.6 ms | 27.4 ms | 34.3 ms | 1.75 |

Everything not listed is within 2% of the step 5 run. What the numbers say: (1) JSON
parsing, which got slower when `Data.Sequence` was replaced (7.2 µs to 10.2 µs), did not
move at all with a list whose pushes are twice as fast, so the list structure is not the
cause; it is still open (ideas). (2) `Set.fromList` and `Map.fromList` are 14 to 16%
slower than on the old Deque and the same as they were on `Data.Sequence` (31.4, 29.0 µs).
(3) With the JIT on, `Map.fromList` reads 1.13× the interpreter (1.09× in a second run;
it was 1.07×). It is interpreted in both modes, and its only exits are the benchmark
harness calling it (`repeat`'s call to its argument), the shape step 8 found to differ by
7 to 15% between runs for no reason found. (4) `Remote.Ref` with the JIT on read 34.3 ms, then 61.9 ms and 28.4 ms in two more
runs, against 26 to 27 ms before and 19.6 ms interpreted. It is a single run of 20 ms
early in the suite, and what varies is whether a major GC lands inside it: with
`+RTS -S`, the 28 ms run has none in its window, and a major GC at that point (about
90 MB live, the loaded codebase) takes 28 ms. Run by itself (`run localCloud`) it reads
35 to 65 ms with the JIT on, because then the JIT's own startup (LLVM, the layout
checks) and the major GC that startup's allocation brings forward both fall inside it;
native lists on or off makes no difference. So this entry measures where collections
fall, not the code, and is no use as a one-shot timing.

`jitSuite` (`off`, then `on`): 66.9 ms / 320 µs, 1.39 ms / 41.2 µs, 86.7 µs / 17.2 µs,
87.3 µs / 4.87 µs, 1.70 ms / 224 µs, 786 µs / 62.4 µs, 1.17 ms / 48.6 µs, 849 µs / 51.2 µs,
7.84 ms / 68.9 µs, text append 1.51 ms / 556 µs, text drop 11.9 ms / 3.56 ms. None of
these use list primitives in their loops, and none moved.

Exits: the list section of the test transcript takes no exit except preemption polls.
