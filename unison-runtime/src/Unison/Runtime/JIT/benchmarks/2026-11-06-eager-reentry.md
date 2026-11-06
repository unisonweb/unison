# 2026-11-06: eager mode generates re-entry functions on demand

**What happened.** PR 6330's second merge-queue run passed every job but took 2 h 15 min
(the run of 2026-11-03 took 52 min), and GitHub removed the pull request from the queue
after its 60-minute check timeout, without a comment. The jit jobs were three to five
times slower on every platform:

| jit job (step: the test transcript under eager; the matrix rows) | 2026-11-03, trunk at 25234f241 | 2026-11-06, 3ef9a303a |
| --- | --- | --- |
| ubuntu-24.04 | 4 min; 14 min | 12 min; 27 min |
| ubuntu-24.04-arm | 4; 12 | 21; 51 |
| macos-15 | 4; 11 | 19; 41 |
| macos-15-intel | 10; 26 | 30; 66 |
| windows-2025 | 4; 14 | 20; 51 |

**Cause.** The per-module compile lines (`UNISON_JIT_LOG=1`) showed the same modules,
three times the IR: the largest, 43 functions, went from 31 MB to 95 MB and from 54 s to
230 s on the ubuntu runner. Locally, with the optimized builds, `jit-tests.md` under eager:

| | trunk 25234f241 | branch aa2db2b20 | `-wide` | `none` | no register entry |
| --- | --- | --- | --- | --- | --- |
| wall time | 56 s | 209 s | | | |
| compile time, summed | 49 s | 201 s | 86 s | 73 s | 201 s |
| LLVM parse and optimize | 23 s | 125 s | 43 s | 38 s | 124 s |
| IR | 224 MB | 618 MB | 365 MB | 297 MB | 618 MB |
| largest module | 9.3 s, 30 MB | 48 s, 92 MB | 16 s, 43 MB | 16 s, 43 MB | 49 s, 92 MB |

The largest module (`unison_2267`) is not a test but the term ucm evaluates a stanza's
watch expressions with (`Runtime.evaluateWatches`: one let rec whose body is a tuple of
every watch; the Records section has 43, Universal equality 38, Every arity 35): 43 `Let`s
in sequence, each a call of a watch's nullary definition. Its 42 `Let` bodies are combinators of the group, each the
function's whole suffix from its `Let`, and each 2.7 MB of IR (1.4 MB without the wide
convention). Eager mode generated every one of them with the group (`groupUnits` with
`lazy` off), so an n-`Let` function cost about n²/2 of its code; the wide convention
doubled each suffix, and the two multiplied. `on` mode has always left the bodies to be
asked for.

**Change.** Eager splits a group as `on` does: the entry points, the local functions and
the re-entry functions that are certain to be used are compiled as the definition loads;
the `Let` bodies and the continuations after slow paths wait to be asked for, and a request
is served on the spot, on the thread that made it (`compileUnitNow`), under a lock since
two Unison threads could ask at once. The threshold is 1 under eager unless
`UNISON_JIT_THRESHOLD` says otherwise, so a body is generated at its first miss, and the
interpreter reads the cell again after counting the miss and enters the body it finds there
(`leap` on a `Push` frame, and the `CallOut` case of `runNativeFrom`), so the body runs
natively from the miss that asked for it.

That last step came from a trace (`UNISON_JIT_TRACE=1`) of the Records section alone, after
the first version of the change: the stanza's tuple ran natively, its first watch called out
(a `Universal.<`, no native version), the exit unwound through the tuple's `Let`, and from
there the interpreter walked the tuple, entering each remaining watch natively and popping each
`Let`'s frame with an empty body cell: 36 suffixes asked for, compiled, and never run, with 6
exits in the whole stanza. Entering a body as soon as it is served leaves one suffix per
call-out instead (5 for that section).

**After**, the same transcript on the optimized build:

| | trunk | before | bodies on demand, interpreted at the miss | and entered at the miss |
| --- | --- | --- | --- | --- |
| wall time | 56 s | 209 s | 100 s | 54 s |
| modules | 1248 | 1440 | 1908 (478 re-entry functions on demand, 1992 never asked for) | 1727 (297 on demand, 2164 never asked for) |
| compile time, summed | 49 s | 201 s | 91 s | 45 s |
| LLVM parse and optimize | 23 s | 125 s | 61 s | 27 s |
| IR | 224 MB | 618 MB | 236 MB | 121 MB |
| largest module | 9.3 s, 30 MB | 48 s, 92 MB | 1.6 s, 2.5 MB | 1.6 s, 2.5 MB |

The output is identical in every mode, and the matrix rows CI runs (`on-t1 stress-all
install-all no-irhelpers`) pass on the fast build in 8.5 min (55 s, 205 s, 46 s, 207 s;
10.5 min after the first version, 60 s, 266 s, 42 s, 257 s).

**What remains.** A suffix per call-out in the watch tuples, each the function's whole rest
(the linear re-entry code of
[optimization-ideas](../optimization-ideas.md#linear-re-entry-code-for-all-cases), a
re-entry function that ends in a call through the next `Let`'s cell instead of the inlined
rest), and the wide convention's own doubling of every function (`-wide` above). Neither
is pressing: the run is at trunk's time with half of trunk's IR.
