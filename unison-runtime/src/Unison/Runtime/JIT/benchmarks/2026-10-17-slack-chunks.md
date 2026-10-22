# Chunks with slack: append in place (2026-10-17)

Appending a piece to a chunk copied the chunk: a 64-character chunk built one character at
a time cost 1 + 2 + … + 64 = 2,080 characters copied and 64 allocations, about 32 copies per
character. Now a chunk that a join made has slack: its array is over-allocated, word 0 of the
array holds the used length, the chunk records the array's capacity, and an append to the
chunk that owns the array's tail (its end is the used length) advances the used length by
compare-and-swap and writes in place; the loser of a race, or a chunk with no slack (a
slice, a literal) or no room, copies into a fresh array with slack. The slack starts at
`slackStart` payload bytes and quadruples per copy up to 512; a join of more than 512 bytes
gets an exact array. Haskell (`joinChunks` in `Unison.Util.Text` and `Unison.Util.Bytes`)
and C (`chunk_join` in `jit_rt.c`) do the same CAS on the same word, and C learns the two
sizes from `rope_init`. The chunk layouts grew a word (`ix_cap`); nothing on the wire changed.

Tests: random append, cons, take and drop sequences on `Text` and `Bytes` against the
`Data.Text` and list models with every earlier version re-checked after each step, two
appends to one value, appends from 64 threads at once, and the startup stress checks at
300,000 random operations through the C helpers.

## Timings

Optimized build, `jit-benchmarks.md`, one round per setting; ms per iteration; before is
b5cbbe098 (bytes as a closure constructor, same day), the three columns are `slackStart` 8,
16 and 32 payload bytes (set through an environment variable for the experiment, since
removed).

Interpreter:

| row | before | 8 | 16 | 32 |
| --- | --- | --- | --- | --- |
| Text: append "hi" 10000 times | 1.01 | 0.917 (-9%) | 0.913 (-9%) | 0.91 (-10%) |
| Bytes: append 2 bytes 10000 times | 0.974 | 0.868 (-11%) | 0.868 (-11%) | 0.866 (-11%) |
| Text: drop 1, 100000 times | 9.07 | 9.1 (+0%) | 9.03 (-1%) | 9.05 (-0%) |
| Bytes: drop 1, 100000 times | 8.23 | 8.32 (+1%) | 8.23 (+0%) | 8.3 (+1%) |
| Text: uncons walk over 100000 characters | 10.4 | 10.4 (+0%) | 10.5 (+1%) | 10.4 (-0%) |
| Nat.toText and Nat.fromText, 10000 times | 7.94 | 7.88 (-1%) | 7.88 (-1%) | 8 (+1%) |
| Bytes: decodeNat64be walk over 80000 bytes | 1.53 | 1.52 (-0%) | 1.51 (-1%) | 1.52 (-0%) |

Native (`--jit on`):

| row | before | 8 | 16 | 32 |
| --- | --- | --- | --- | --- |
| Text: append "hi" 10000 times | 0.239 | 0.2 (-16%) | 0.2 (-16%) | 0.2 (-17%) |
| Bytes: append 2 bytes 10000 times | 0.237 | 0.201 (-15%) | 0.197 (-17%) | 0.197 (-17%) |
| Text: drop 1, 100000 times | 1.88 | 1.9 (+1%) | 1.93 (+3%) | 1.89 (+1%) |
| Bytes: drop 1, 100000 times | 1.82 | 1.85 (+2%) | 1.82 (+0%) | 1.82 (-0%) |
| Text: uncons walk over 100000 characters | 3.36 | 3.36 (+0%) | 3.36 (+0%) | 3.32 (-1%) |
| Nat.toText and Nat.fromText, 10000 times | 0.719 | 0.71 (-1%) | 0.71 (-1%) | 0.708 (-1%) |
| Bytes: decodeNat64be walk over 80000 bytes | 0.472 | 0.463 (-2%) | 0.468 (-1%) | 0.459 (-3%) |

The append rows are 16 to 17% faster native and 9 to 11% interpreted: the copy of up to 64
bytes per append is gone, and what is left of an append is the chunk object, the rope spine
and (interpreted) the dispatch, which the row measures at 20 ns native. The starting slack
makes no measurable difference, so it is 8, the smallest asked for: a text built by two
appends has an 8-byte array behind it. Every other row is within noise, as expected: nothing
but the join changed. The rest of the suite (not shown) is within 3%.
