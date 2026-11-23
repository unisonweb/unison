# `specialize`: interpreted, specialized and by hand

Branch `specialize` at `0efffb1e3` plus the higher-order rows of
[`specialize-benchmarks.md`](../../../../../../unison-src/transcripts-manual/specialize-benchmarks.md),
optimized build (`--work-dir .stack-work-opt`), Apple M1, the `jit` testing codebase. One
run of `printTime` per row; the specialization happens before the timing. See
[Specialize.md](../../Specialize.md).

| Row | JIT off | JIT on |
| --- | --- | --- |
| fib 20: interpreted | 16.3 ms | 744 µs |
| fib 20: specialized | 1.71 ms | 30.9 µs |
| fib 20: by hand | 1.72 ms | 36.8 µs |
| polynomial, 100000 points: interpreted | 134 ms | 6.54 ms |
| polynomial, 100000 points: specialized | 15.4 ms | 553 µs |
| polynomial, 100000 points: by hand | 15.0 ms | 484 µs |
| map with a lambda, 10000 elements: generic | 1.42 ms | 255 µs |
| map with a lambda, 10000 elements: specialized | 1.19 ms | 232 µs |
| foldLeft with a lambda, 10000 elements: generic | 1.08 ms | 168 µs |
| foldLeft with a lambda, 10000 elements: specialized | 684 µs | 154 µs |
| foldLeft with a lambda, 10000 elements: by hand | 419 µs | 155 µs |
| filter and map as a fold, 10000 elements: generic | 1.91 ms | 251 µs |
| filter and map as a fold, 10000 elements: specialized | 1.14 ms | 153 µs |
| filter and map as a fold, 10000 elements: by hand | 944 µs | 160 µs |
| five functions from a list, 100000 points: generic | 88.5 ms | 11.3 ms |
| five functions from a list, 100000 points: specialized | 15.6 ms | 611 µs |
| five functions from a list, 100000 points: by hand | 15.2 ms | 485 µs |
| fib 20, interpreter in CPS: interpreted | 27.9 ms | 3.50 ms |
| fib 20, interpreter in CPS: specialized | 2.52 ms | 274 µs |

Findings:

- Specializing an interpreter on a program gives about 10× under the interpreter and 12–24×
  under the JIT, and lands within 15% of the program written by hand (fib under the JIT is
  within noise of it).
- A function argument known to a loop is inlined into it: with the JIT on, `foldLeft` and the
  filter-and-map fold match the hand-written loops, and a known list of five functions
  applied in turn becomes straight-line code, 18× faster than the generic version.
- `map` gains least (10–15%): building the list dominates.
- With the JIT off, the specialized `foldLeft` is 1.6× the hand-written loop although the JIT
  makes them equal: the output carries interpretive overhead the JIT removes (the peeled
  first round, literal lets). Not looked into yet.
- The interpreter in continuation-passing style keeps its continuations as closures after
  specializing (the whistle generalizes them), so it gains less than the direct-style one
  relative to its starting point, but still about 10×.
