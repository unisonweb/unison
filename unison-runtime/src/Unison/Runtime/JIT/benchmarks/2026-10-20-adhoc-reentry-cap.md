# 2026-10-20: the cap on ad hoc re-entry functions

A `Reenter` or `GrowStack` exit in a callee unwinds every native caller on the C
stack, and each caller's `Let` became a `Push` frame whose empty cell counted a miss
when popped, so in `on` mode a long-running function was in time interrupted at every
Let it has and got a copy of its suffix after each. The trampoline now tells those
exits apart from the ones the code causes when it builds the frames (`pushRecord`):
such a frame carries no cell, and `JIT.noteAdHocExit` counts the miss instead, letting
at most `UNISON_JIT_REENTRY_ADHOC` (3) re-entry functions per group be asked for this
way. See "Re-entry functions on demand" in design.md.

The change is on the exit path only: one `isAdHoc` test per exit and one flag test per
frame record, and the cell is read and the budget consulted only for an ad hoc exit
through a Let whose body has no code. `leap` and `countReentry`, the hot path, are
untouched. This run checks that the suite sees nothing.

Optimized build (`.stack-work-opt`), `--jit on`, `jit-benchmarks.md` through
`transcript.fork`, against the "hoisted" column of
[2026-10-19 JIT handle](2026-10-19-jit-handle.md) (same machine, quiet). Two runs.
Every row is within the suite's usual scatter: the two at 1.04× ("List: cons, then a
pattern walk" and "Store whose put requests an outer Counter") moved by as much between
the columns of the 2026-10-19 record, and "Text: append" is the row
[2026-10-18](2026-10-18-compiled-programs.md) found bimodal.

| 2026-10-19 | run 1 | run 2 | run 2 / 2026-10-19 | Benchmark |
| --- | --- | --- | --- | --- |
| 319.266µs | 318.615µs | 321.62µs | 1.01× | Sum 0 to 1 million |
| 35.399µs | 35.356µs | 35.354µs | 1.00× | fib 20 |
| 13.79µs | 13.712µs | 13.569µs | 0.98× | Cons list: map with a lambda (1000 elements) |
| 4.824µs | 4.888µs | 4.83µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 98.552µs | 97.689µs | 96.003µs | 0.97× | Binary tree: 1000 inserts |
| 38.049µs | 38.377µs | 39.182µs | 1.03× | Binary tree: 1000 lookups |
| 57.849µs | 57.584µs | 57.895µs | 1.00× | Apply a function argument 10000 times |
| 41.216µs | 41.847µs | 41.906µs | 1.02× | Mutate a Ref 10000 times |
| 66.467µs | 66.983µs | 66.368µs | 1.00× | Calls across definitions: Collatz steps for 1 to 1000 |
| 118.579µs | 118.462µs | 117.958µs | 0.99× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 198.38µs | 202.43µs | 211.663µs | 1.07× | Text: append "hi" 10000 times |
| 1.874872ms | 1.924144ms | 1.900593ms | 1.01× | Text: drop 1, 100000 times |
| 194.988µs | 199.467µs | 200.292µs | 1.03× | Bytes: append 2 bytes 10000 times |
| 1.793789ms | 1.81701ms | 1.803202ms | 1.01× | Bytes: drop 1, 100000 times |
| 2.150903ms | 2.130292ms | 2.133835ms | 0.99× | Bytes: at, 100000 times |
| 3.262471ms | 3.327328ms | 3.328662ms | 1.02× | Text: uncons walk over 100000 characters |
| 285.834µs | 298.695µs | 295.927µs | 1.04× | List: cons 10000 elements, then a pattern walk |
| 151.678µs | 148.901µs | 147.724µs | 0.97× | List: at, 10000 times |
| 712.456µs | 708.578µs | 713.012µs | 1.00× | Nat.toText and Nat.fromText, 10000 times |
| 1.004107ms | 1.002272ms | 1.004178ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 490.199µs | 488.589µs | 486.037µs | 0.99× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 539.235µs | 538.647µs | 538.442µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 44.425µs | 45.134µs | 44.283µs | 1.00× | MutableArray: fill, freeze and sum 10000 elements |
| 126.702µs | 128.471µs | 129.577µs | 1.02× | Ref.cas loop, 10000 times |
| 274.341µs | 283.475µs | 278.718µs | 1.02× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.415562ms | 7.64214ms | 7.487559ms | 1.01× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.033053ms | 5.020837ms | 5.026356ms | 1.00× | Records: a 16-field literal and four getters, 100000 times |
| 3.358153ms | 3.333384ms | 3.3295ms | 0.99× | Counter: 100000 requests |
| 6.259456ms | 6.417806ms | 6.497175ms | 1.04× | Store: get and put, 100000 iterations |
| 4.439521ms | 4.400959ms | 4.392328ms | 0.99× | handle installed per iteration, 10000 iterations |
| 10.210412ms | 10.365065ms | 10.610775ms | 1.04× | Store whose put requests an outer Counter, 100000 iterations |
| 463.273µs | 470.082µs | 465.715µs | 1.01× | Bytes: decodeNat64be walk over 80000 bytes |

The mechanism itself, on the fast build with `jit-tests.md`, `on` mode, threshold 1 and
the poll every third entry (`UNISON_JIT_STRESS=poll=3`), output identical in every run:

| cap | re-entry functions asked for by ad hoc exits | ad hoc exits through an uncompiled Let past the cap |
| --- | --- | --- |
| 0 | 0 | 3,129,247 |
| 1 | 14 | 59,357 |
| 3 (default) | 13 | 0 |
| 1000 | 13 | 0 |

No group of the test transcript has more than three Lets the poll interrupts, so the
default never refuses there; the cap of 1 shows it refusing. Plain `on` mode asks for 4
this way over the transcript, `on` with threshold 1 for 8.
