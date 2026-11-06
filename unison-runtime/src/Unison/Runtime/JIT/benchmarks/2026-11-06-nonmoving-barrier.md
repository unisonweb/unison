# 2026-11-06: the non-moving collector's write barrier

The JIT generates the snapshot write barrier the non-moving collector (`+RTS -xn`) needs
(design.md, "The write barrier"): before every pointer store into `bstk` or an array, when the
RTS's barrier flag was set at entry, the old value goes to the update remembered set through
`unison_jit_push_old`; the exit routine, `unison_jit_enter`, `Ref.write` and `Ref.cas` do the
same in C. Nothing is generated unless the process runs under `-xn`; under the default collector
the only change is a flag test in the C `Ref` write and one more field written at entry.

Optimized build (`.stack-work-opt`), macOS arm64, the binary built before the barrier against
the one with it, interleaved, two runs each: `jit-benchmarks.md` with `UNISON_JIT=on`, and
`jit-suite.md` with `UNISON_JIT=eager`, each under the default collector and under `+RTS -xn`.
The before binary has no barrier at all, which is fine for timing. (The binary measured is one
change older than the commit: `UNISON_JIT_DISABLE=nmbarrier` since also turns the C helpers'
pushes off, one load at entry.)

## `jitSuite`, default collector: unchanged

Every row within noise (0.98x to 1.02x) except:

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Mutate a Ref 10000 times | 0.033 | 0.037 | 0.88x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.461 | 0.482 | 0.96x |

The Ref row is 4 µs over 10,000 writes: the C `Ref.write` now tests the barrier flag. The
Bytes row moved the same amount under both collectors and is probably layout.

## `jitSuite`, `+RTS -xn`: within noise

Every row 0.97x to 1.06x except the two list-of-lambda rows at 0.88x and 0.91x (0.011 and 0.015
ms rows, a microsecond each) and the Ref row at 0.94x. The per-store branch doesn't show; the
collector itself costs the Store and handler rows 3% against the default collector, before and
after alike.

## The library suite (`jit-suite.md`, eager)

Default collector, two runs: within noise everywhere; `Json complex decoding` 0.91x (0.036 to
0.040 ms), `Remote.Ref` 1.21x (the row that varies between runs). Under `+RTS -xn` the before
binary crashed in both runs, after two minutes, in the marker thread:

    internal error: mark_closure: unimplemented/strange closure type 1293789184 @ 0x70435f7258

a swept object reached from a slot written during the mark. The binary with the barrier
finished both runs with the same results as under the default collector; its `-xn` rows vary
more from run to run (three rows 0.5x to 0.8x between the two runs) than before against after.

## The stress test

`jit-nonmoving.md` (development.md, "Running"), optimized build, `-xn -A4m -F1.1`, about fifty
marks during the loop: with the barrier it prints the checksum in 18 s; the control
(`UNISON_JIT_DEBUG=1 UNISON_JIT_DISABLE=nmbarrier`, which turns the generated and the C pushes
off) crashed in the marker in three runs out of three, one of them after printing a wrong
checksum; with 500,000-element arrays instead of 50,000 the control printed a wrong checksum in
three runs out of three and the before binary did so twice and crashed once.
