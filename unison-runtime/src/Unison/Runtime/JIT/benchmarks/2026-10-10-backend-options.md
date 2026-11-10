# A faster instruction selector and a cheaper register allocator (2026-10-10)

The [backend profile](2026-10-10-backend-profile.md) put instruction selection and register
allocation at about half of code generation. LLVM has a second instruction selector,
GlobalISel, built to run faster than the default SelectionDAG, and a simpler register
allocator, `basic`, than the default `greedy`. Both are a flag away through
`UNISON_JIT_LLVM_OPTS` (`-global-isel -global-isel-abort=0`, `-regalloc=basic`), so this
measures them on the suite: optimized build, `--jit on`, two runs of each configuration and
five of the base (the two of this day and the three of the [exit call](2026-10-09-exit-call.md)).
GlobalISel handled every function of the modules checked without falling back (`llc
-global-isel-abort=1` on the dumps), so its numbers are its own.

In the harness, on the largest module, GlobalISel saved 5% of code generation, the basic
allocator 10%, both together 18%; on a small module about 10% each.

## Compile totals

| | base | gisel | basic | both |
| --- | --- | --- | --- | --- |
| modules | 74–74 | 74–74 | 74–74 | 74–74 |
| IR KB | 2887–2898 | 2891–2898 | 2889–2898 | 2891–2898 |
| compile time, ms | 876–1076 | 841–863 | 822–826 | 789–794 |
| generate | 83–122 | 84–84 | 84–84 | 84–84 |
| parse and optimize | 266–399 | 266–273 | 265–269 | 267–270 |
| code generation | 524–555 | 490–506 | 472–473 | 437–439 |

## Timings, median of the runs, relative to base

| benchmark | base | gisel | basic | both |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 320 µs | -0% | -0% | +0% |
| fib 20 | 35.5 µs | -1% | +4% | +4% |
| Cons list: map with a lambda (1000 elements) | 13.1 µs | +2% | +7% | +7% |
| Cons list: foldLeft with a lambda (1000 elements) | 4.87 µs | -0% | -0% | -0% |
| Binary tree: 1000 inserts | 189 µs | -1% | +2% | +7% |
| Binary tree: 1000 lookups | 46 µs | +5% | +5% | +6% |
| Apply a function argument 10000 times | 48.2 µs | +0% | +0% | +1% |
| Mutate a Ref 10000 times | 35.3 µs | -0% | +1% | +1% |
| Calls across definitions: Collatz steps for 1 to 1000 | 66.6 µs | +7% | +2% | +6% |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 118 µs | -0% | +0% | +0% |
| Text: append "hi" 10000 times | 239 µs | +3% | +1% | +2% |
| Text: drop 1, 100000 times | 2.03 ms | -1% | -1% | +4% |
| Bytes: append 2 bytes 10000 times | 229 µs | -0% | +2% | +4% |
| Bytes: drop 1, 100000 times | 2.15 ms | -3% | -1% | -5% |
| Bytes: at, 100000 times | 2.19 ms | -0% | +0% | +1% |
| Text: uncons walk over 100000 characters | 3.81 ms | +2% | -1% | -3% |
| Nat.toText and Nat.fromText, 10000 times | 706 µs | -0% | -0% | -0% |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.01 ms | -8% | +0% | -8% |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 4.17 ms | +0% | +1% | +0% |
| MutableArray: fill, freeze and sum 10000 elements | 44.4 µs | -0% | +1% | +2% |
| Ref.cas loop, 10000 times | 118 µs | -0% | +0% | -0% |
| murmurHashUntyped of Some (i, "x"), 10000 times | 259 µs | +0% | +2% | +8% |
| Bytes: decodeNat64be walk over 80000 bytes | 424 µs | +0% | -0% | +3% |

## Outcome

Each saves 7–10% of code generation (3–5% of the compile time) and costs the hot loops:
GlobalISel makes Collatz 7% and lookups 5% slower (and the float row 8% faster), the basic
allocator fib 4%, `Cons.map` 7% and lookups 5% slower, both together 6–8% on several rows.
The per-run values are consistent, so these are real. The basic allocator's output has 2%
more instructions and 7% more spills and reloads on the largest module. Neither is worth it
at the standard of this work (compile time without compromising the generated code); the
defaults stay. The knob remains for anyone who wants to trade 5% of compile time for 5% of
run time on purpose.
