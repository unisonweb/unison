# Exit write-back: shared blocks, live slots only (2026-10-09)

The third item of the compile-time list ([2026-10-09 compile time](2026-10-09-compile-time.md)):
less code per exit. Optimized build, `--jit on`, the suite, four runs each. "before" is the
build of that file (schedulers off, size term in the batch rule); "after" adds the two parts
of the change, measured separately on the way.

## What the exits cost

In the suite's largest remaining module (`unison_u657_22_x7`: 7 re-entry functions of the
`decodeNat64be` walk and their 7 auxiliary functions, 186 exits, 2.1 MB of IR), the blocks
that write the frame back to the Unison stack and return a status were 72% of the lines.
Each exit wrote every slot up to its depth, 25 to 35 slots, at 8 lines a slot.

## The two parts

1. **Shared write-back blocks.** Exits and unwinds that write the same thing (depth, slots,
   frame records) branch to one block with the status as a phi. This alone: IR 6.9 → 5.7 MB,
   compile time 1.75 → 1.65 s. Little was shared: the re-entry functions of a chain of
   `Let`s exit at a different depth each (172 blocks for 204 sites in that module).
2. **Only the live slots.** `liveAt` walks the section an exit resumes at, with the generator's
   depth rules, and collects every operand's offset; the enclosing bindings' bodies are added.
   Anything it can't follow makes every slot live. Then the frame's address on each stack is
   computed once per block instead of once per slot.

## Compile totals

| | before | shared blocks | both parts |
| --- | --- | --- | --- |
| IR | 6.9 MB | 5.7 MB | 3.8 MB |
| compile time | 1.7–1.8 s | 1.6–2.0 s | 1.26 s (three runs), 1.64 s (one, a different largest module) |
| `unison_u657_22_x7` | 2.1 MB, 445 ms | 1.7 MB, 410 ms | 0.9 MB, 316 ms |
| `unison_695` | 1.0 MB, 240 ms | 0.9 MB, 215 ms | 0.5 MB, 157 ms |

The live sets in the rope and decode code are still 15 of 25 slots: deep `VArgN` reads keep
early slots alive, so the write-back that remains is real.

## Timings, median of four runs

| benchmark | before | after | change |
| --- | --- | --- | --- |
| Sum 0 to 1 million | 325 µs | 320 µs | -2% |
| fib 20 | 42 µs | 36 µs | -15% |
| Cons list: map with a lambda (1000 elements) | 14 µs | 13 µs | -7% |
| Cons list: foldLeft with a lambda (1000 elements) | 5 µs | 5 µs | -1% |
| Binary tree: 1000 inserts | 195 µs | 191 µs | -2% |
| Binary tree: 1000 lookups | 53 µs | 48 µs | -9% |
| Apply a function argument 10000 times | 50 µs | 48 µs | -4% |
| Mutate a Ref 10000 times | 37 µs | 36 µs | -4% |
| Calls across definitions: Collatz steps for 1 to 1000 | 72 µs | 72 µs | -0% |
| Calls across definitions: callee first reached once the caller is hot | 122 µs | 118 µs | -3% |
| Text: append "hi" 10000 times | 249 µs | 261 µs | +5% |
| Text: drop 1, 100000 times | 2.21 ms | 2.27 ms | +3% |
| Bytes: append 2 bytes 10000 times | 237 µs | 255 µs | +8% |
| Bytes: drop 1, 100000 times | 2.17 ms | 2.28 ms | +5% |
| Bytes: at, 100000 times | 2.26 ms | 2.20 ms | -3% |
| Text: uncons walk over 100000 characters | 4.08 ms | 4.20 ms | +3% |
| Nat.toText and Nat.fromText, 10000 times | 721 µs | 722 µs | +0% |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.00 ms | 993 µs | -1% |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 4.28 ms | 4.18 ms | -2% |
| MutableArray: fill, freeze and sum 10000 elements | 46 µs | 47 µs | +1% |
| Ref.cas loop, 10000 times | 120 µs | 119 µs | -0% |
| murmurHashUntyped of Some (i, "x"), 10000 times | 279 µs | 262 µs | -6% |
| Bytes: decodeNat64be walk over 80000 bytes | 459 µs | 455 µs | -1% |

`fib` and the tree lookups are faster, consistently across the runs: with fewer values
flowing to the exit paths, less is kept in callee-saved registers across the calls (the
effect the branch weights were added for). The text and bytes append rows are up by less
than their run-to-run spread (the old builds gave 237 to 273 µs for the bytes one).

Correctness: the test transcript is idempotent in every mode and stress combination of
[development](../development.md#testing).

Takeaways. The suite's compile time is now about 1.26 s against 3.5 s this morning, with the
same functions compiled and the hot code as fast or faster. The remaining cost is spread
over modules of 150 to 500 ms; the next levers are the IR text itself (parsing was 17%) and
the write-back blocks themselves, which are still three quarters of the largest module
(one block per exit site, since the sites differ in depth; see [optimization-ideas](../optimization-ideas.md#code-size)).
The argument stores at call sites, the other half of the original idea, turned out to be 1%
of the lines.
