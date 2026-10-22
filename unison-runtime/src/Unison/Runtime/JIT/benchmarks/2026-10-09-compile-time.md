# Compile time: schedulers off, size in the batch rule (2026-10-09)

Where the time went, and what two changes did to it. Optimized build, `--jit on`, the suite
(`jit-benchmarks.md`), four runs of the old behaviour and of the new, two of the size rule
alone. The build used for this comparison keeps the old behaviour reachable through
`UNISON_JIT_SCHED=1` (keep LLVM's machine schedulers) and `UNISON_JIT_SIZE_UNIT=100000000`
(no size term in the batch rule).

## Where a module's time goes

The suite's largest module (`unison_839`: 35 definitions, 72 functions, 3.8 MB and 96k lines
of IR, 1.2 s in the runtime), dumped with `--jit-dump-ir` and re-run offline with Homebrew's
`opt` and `llc` 23:

| phase | time |
| --- | --- |
| parsing the IR text | 0.24 s |
| `default<O2>` (O1: 0.16 s) | 0.21 s |
| backend, default level | 0.94 s |
| backend with both machine schedulers off | 0.36 s |
| backend at O0 (fast register allocator) | 0.10 s |

In the backend (`llc -time-passes`) the pre-register-allocation machine scheduler is 44% and
the post-RA scheduler 29%; instruction selection 12%, register allocation 4%. Two functions
are 85% of the module: the body of `jitSuite` itself (183 lines of MCode, frame size 128, 56
call sites, 55k lines of IR) and the harness's row function (27k lines). The suite body runs
once; it joined the batch because its 56 sites into hot callees add up past the gate of 25
estimated calls. The instruction mix is 33k loads, 20k GEPs, 18k stores and 17k adds against
600 branches and 277 calls: slot traffic to and from the real stack around call sites and
exits, in a few very long basic blocks, which is the shape the schedulers are superlinear in.

## The two changes

1. The machine schedulers are off (`-enable-misched=false -enable-post-misched=false`
   through `LLVMParseCommandLineOptions`, in `jit_llvm.c` right after LLVM is loaded).
2. A candidate's estimated calls are divided by its size relative to a typical definition
   before the gate is applied (`groupSize` in `JIT.hs`: frame size times call sites, summed
   over the group; the unit is `UNISON_JIT_SIZE_UNIT`, 500). A candidate hot on its own count
   joins regardless.

## Compile totals (`UNISON_JIT_STATS`)

| | old | size rule | both |
| --- | --- | --- | --- |
| compile time, per run | 3.9, 3.2, 3.6, 3.1 s | 2.5, 2.2 s | 1.8, 1.7, 1.7, 1.8 s |
| functions compiled | 119–120 | 94 | 94 |
| re-entry functions never asked for | 301–306 | 110 | 110–112 |
| IR | 10.5 MB | 6.9 MB | 6.9 MB |
| largest module | `unison_839`, 3.7 MB, 1.14–1.17 s | 2.1 MB, 0.59–0.64 s | 2.1 MB, 0.44–0.48 s |

The size rule leaves the suite body and the row function interpreted (25 fewer functions, a
third of the IR, and 190 fewer re-entry functions generated for nothing), and the largest
module is now a re-entry batch of the `Bytes.decodeNat64be` walk. The schedulers are worth
about a third of what is left.

## Timings, median of the runs

| benchmark | old | size rule | both | both vs old |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 334 µs | 329 µs | 325 µs | -3% |
| fib 20 | 43 µs | 42 µs | 42 µs | -2% |
| Cons list: map with a lambda (1000 elements) | 15 µs | 15 µs | 14 µs | -4% |
| Cons list: foldLeft with a lambda (1000 elements) | 5 µs | 5 µs | 5 µs | -5% |
| Binary tree: 1000 inserts | 207 µs | 213 µs | 195 µs | -6% |
| Binary tree: 1000 lookups | 52 µs | 51 µs | 53 µs | +2% |
| Apply a function argument 10000 times | 50 µs | 51 µs | 50 µs | +2% |
| Mutate a Ref 10000 times | 38 µs | 39 µs | 37 µs | -2% |
| Calls across definitions: Collatz steps for 1 to 1000 | 68 µs | 69 µs | 72 µs | +5% |
| Calls across definitions: callee first reached once the caller is hot | 121 µs | 122 µs | 122 µs | +1% |
| Text: append "hi" 10000 times | 268 µs | 293 µs | 249 µs | -7% |
| Text: drop 1, 100000 times | 2.26 ms | 2.34 ms | 2.21 ms | -2% |
| Bytes: append 2 bytes 10000 times | 255 µs | 273 µs | 237 µs | -7% |
| Bytes: drop 1, 100000 times | 2.10 ms | 2.29 ms | 2.17 ms | +3% |
| Bytes: at, 100000 times | 2.34 ms | 2.33 ms | 2.26 ms | -3% |
| Text: uncons walk over 100000 characters | 3.80 ms | 4.00 ms | 4.08 ms | +7% |
| Nat.toText and Nat.fromText, 10000 times | 743 µs | 738 µs | 721 µs | -3% |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.00 ms | 1.01 ms | 1.00 ms | +0% |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 4.30 ms | 4.32 ms | 4.28 ms | -0% |
| MutableArray: fill, freeze and sum 10000 elements | 46 µs | 49 µs | 46 µs | +1% |
| Ref.cas loop, 10000 times | 122 µs | 124 µs | 120 µs | -2% |
| murmurHashUntyped of Some (i, "x"), 10000 times | 265 µs | 275 µs | 279 µs | +5% |
| Bytes: decodeNat64be walk over 80000 bytes | 465 µs | 464 µs | 459 µs | -1% |

Everything is within the suite's run-to-run noise (single runs of the same setting differ by
up to 40% on the hash and decode rows). The scheduler change moved nothing measurable in the
generated code; the row that is up the most (`uncons` walk, +7%) is up under the size rule
alone as well, so it isn't the schedulers.

Takeaways. Compile time on the suite halved, from about 3.5 s to 1.75 s, with the code for
hot functions unchanged. What remains is spread over modules of 0.1 to 0.5 s whose cost is
the volume of slot traffic per call site and exit (the re-entry batch of `decodeNat64be` is
2 MB of IR for 7 functions); that is the next lever, see
[optimization-ideas](../optimization-ideas.md#compile-time).
