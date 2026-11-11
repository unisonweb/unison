# 2026-11-11: exit sites share their stores into the argument block

**What happened.** The merge queue's jit jobs spend most of their time in two rows of the
test matrix, `stress-all` and `no-irhelpers` (macos-15-intel: 1168 s and 941 s of a 52-minute
job on 2026-11-11). Locally on trunk (2d665336e, the fast build), `jit-tests.md` under the
stress combination took 177 s, of which 143 s was compiling; under plain `eager`, 78 s and 58 s.

**Cause.** Every exit and unwind site stored its own live slots into the exit routine's
argument block: two loads, two addresses and two stores per slot. In straight-line code the
live slots grow with the function, so the stores add up to about n² of them. A tuple of n
calls of a recursive function, as one watch expression, under `eager`:

| calls | exits | slot stores | IR | compile time |
| --- | --- | --- | --- | --- |
| 8 | 30 | 626 | 149 KB | 51 ms |
| 16 | 54 | 2,530 | 446 KB | 188 ms |
| 32 | 102 | 10,178 | 1,546 KB | 959 ms |
| 64 | 198 | 40,834 | 5,757 KB | 5,560 ms |

The transcript's largest modules are that shape: the term ucm evaluates a stanza's watch
expressions with (`Runtime.evaluateWatches`) is a tuple of every watch. Module 2269 (42
calls) was 64,098 lines of IR, 56,600 of them these stores (18,888 slot stores).

**The change.** A site with more than four live slots shares its stores with the function's
other sites (design.md, "Write-back"; `Codegen.joinChain`). The same test:

| calls | slot stores | IR | compile time |
| --- | --- | --- | --- |
| 8 | 118 | 85 KB | 36 ms |
| 16 | 198 | 157 KB | 80 ms |
| 32 | 358 | 310 KB | 171 ms |
| 64 | 678 | 658 KB | 437 ms |

`jit-tests.md` on the fast build, each site storing its own (`UNISON_JIT_EXITS=call`) against
the shared form, same binary:

| | `eager`, own | `eager`, shared | stress combination, own | stress combination, shared |
| --- | --- | --- | --- | --- |
| wall time | 99 s | 58 s | 222 s | 99 s |
| compile time, summed | 73.8 s | 33.3 s | 184.2 s | 63.4 s |
| IR | 122 MB | 62 MB | 283 MB | 126 MB |
| largest module | 3,138 KB | 1,495 KB | 3,155 KB | 1,511 KB |

Module 2269 went from 2,807 KB to 415 KB (64,098 lines to 9,028; 18,888 slot stores to 412).
The largest module left (2079, 162 `Let`s in branches) shares less: its live sets differ in
their low slots from arm to arm.

The whole test matrix passes under the shared form (19 rows).

**Why sites with few live slots keep their own stores.** With every site sharing,
`jit-benchmarks.md` on the optimized build, two runs of each form, was flat in the mean
(1.003) but five rows were slower beyond the noise: `fib 20` 1.14x the time (37.1 to
42.3 µs), Collatz steps 1.13x, `map` with a lambda 1.11x, 100,000 calls of a one-step callee
1.10x, tree inserts 1.09x. In `fib`'s optimized code the differences were the phis of the
shared blocks (4 to 14) and the argument block (7 words to 29, since a slot's place is its
number). Which of the two cost the time was not separated.

With the threshold at four slots, `fib`'s sites (1 to 3 slots) are as before. Two clean runs
of each form (the first of three pairs was disturbed: its shared run was 8% slower than the other two on
every row, the own-stores run within 0.2%):

| row | own stores | shared, more than four slots |
| --- | --- | --- |
| `fib 20` | 37.2, 37.2 µs | 37.2, 37.2 µs |
| Collatz steps for 1 to 1000 | 80.0, 76.5 | 77.4, 75.8 |
| `map` with a lambda, 1000 elements | 14.9, 14.5 | 14.5, 14.3 |
| Binary tree: 1000 inserts | 99.2, 94.2 | 94.2, 95.0 |
| geometric mean of the 44 rows, best of two | 1 | 1.007 |

The slowest row is "Loop state: a record updated each round" at 1.07x (77.4 to 82.9 µs);
five more are 1.04x to 1.05x (`Text` and `Bytes` append, `decodeNat64be`, `Store` get and
put, list cons and walk). The run-to-run difference of one form was about 2%. The benchmark
suite's own IR went from 9.6 MB to 6.7 MB.

A threshold of eight was built and not measured to the end; four was chosen for the smaller
code. In the transcript's IR under `eager`, sites with four or fewer live slots are 42% of
the exit descriptors and 6% of the slot stores.

**Not done.** The descriptors still list each site's slots (62 KB of module 2269's 415 KB),
and the generator still walks each site's live slots to find its blocks; both are in
optimization-ideas.md.
