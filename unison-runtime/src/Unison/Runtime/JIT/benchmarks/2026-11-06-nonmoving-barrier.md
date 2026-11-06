# 2026-11-06: the non-moving collector's write barrier

The JIT generates the snapshot write barrier the non-moving collector (`+RTS -xn`) needs
(design.md, "The write barrier"): before every pointer store into `bstk` or an array, when the
RTS's barrier flag was set at entry, the old value goes to the update remembered set through
`unison_jit_push_old`; the exit routine, `unison_jit_enter`, `Ref.write` and `Ref.cas` do the
same in C. Nothing is generated unless the process runs under `-xn`; under the default collector
the only change is a flag test in the C `Ref` write and one more field written at entry.

Optimized build (`.stack-work-opt`), macOS arm64, the binary built before the barrier against
the one with it, interleaved. The before binary has no barrier at all, which is fine for timing.
Two measurements: the first of the barrier as first built (the C helpers tested the flag at
runtime), the second of the committed form (no test on a hot path: the Ref pushes in generated
code, the exit routine reading the flag once).

## `jitSuite`, default collector: unchanged

The committed form, two runs each: every row between 0.98x and 1.03x of the before binary; the
two Ref rows 1.00x. The first form had the "Mutate a Ref" row at 0.88x (4 µs over 10,000 writes:
the C `Ref.write` tested the flag), which is what moved the pushes into generated code.

## `jitSuite`, `+RTS -xn`: within noise

The committed form, one run each: every row 0.95x to 1.06x except two microsecond rows (`Binary
tree: 1000 lookups` 0.89x, `filterMap` 0.86x, 0.011 and 0.014 ms) and `Bytes: drop 1` at 1.09x,
the run-to-run spread of those rows. The first form, two runs each, the same picture. The
per-store branch doesn't show; the collector itself costs the Store and handler rows 3% against
the default collector, before and after alike.

## The library suite (`jit-suite.md`, eager)

The first form, two runs each. Default collector: within noise everywhere; `Json complex
decoding` 0.91x (0.036 to 0.040 ms), `Remote.Ref` 1.21x (the row that varies between runs).
Under `+RTS -xn` the before binary crashed in both runs, after two minutes, in the marker thread:

    internal error: mark_closure: unimplemented/strange closure type 1293789184 @ 0x70435f7258

a swept object reached from a slot written during marking. The binary with the barrier finished
both runs with the same results as under the default collector; its `-xn` rows vary more from
run to run (three rows 0.5x to 0.8x between the two runs) than before against after.

## The stress test

`jit-nonmoving.md` (development.md, "Running"), optimized build, `-xn -A4m -F1.1`, about fifty
marks during the loop: with the barrier it prints the checksum in 18 s; the control
(`UNISON_JIT_DEBUG=1 UNISON_JIT_DISABLE=nmbarrier`, which turns the generated and the C pushes
off) crashed in the marker in three runs out of three, one of them after printing a wrong
checksum; with 500,000-element arrays instead of 50,000 the control printed a wrong checksum in
three runs out of three and the before binary did so twice and crashed once.

## A row that was the verifier's to catch

The first cas push treated a pointer register as an integer, invalid IR: without
`UNISON_JIT_DEBUG=1` the module is rejected silently and the function runs interpreted, and the
`Ref.cas loop` row showed it at 0.12x. With the fix, and a verifier run under both collectors
before measuring, 1.00x.
