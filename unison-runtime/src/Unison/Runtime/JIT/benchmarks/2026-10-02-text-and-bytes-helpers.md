# Text and Bytes helpers

The `jitSuite` rows added as text and bytes went native, optimized build, interpreter against
`UNISON_JIT=on`.

## Bytes natively (2026-10-02)

| Benchmark | Interpreter | JIT |
| --- | --- | --- |
| Bytes: append 2 bytes 10000 times | 1.03 ms | 310 µs |
| Bytes: drop 1, 100000 times | 8.69 ms | 2.77 ms |
| Bytes: at, 100000 times (a `match` on each `Optional`) | 10.1 ms | 7.4 ms |

The first two are what the text versions get (318 µs and 2.66 ms on the same run). `Bytes.at`
gains less: each step allocates a `Some`, matches on it, and the index itself walks the tree
to the chunk (the 100000-byte sample was built two bytes at a time, so it is 1600 chunks of
64; `rope_chunk_at` is O(log n) in that), about 74 ns a step in all. The text and list
checks and the rest of `jitSuite` are unchanged (every other row within 2% of the previous
run).

## The rest of Text and Bytes (2026-10-03)

Three rows added, and one older row moved because `Compile.hs` now puts the `None` constant
in the pool for `Bytes.at` too (it had been falling back to the call-out for want of it).

| Benchmark | Interpreter | JIT |
| --- | --- | --- |
| Text: uncons walk over 100000 characters | 10.9 ms | 4.46 ms |
| Nat.toText and Nat.fromText, 10000 times | 7.95 ms | 803 µs |
| Bytes: decodeNat64be walk over 80000 bytes | 1.49 ms | 596 µs |
| Bytes: at, 100000 times (was 7.4 ms with the JIT) | 10.1 ms | 2.47 ms |

Every other row is within noise of the previous run. The uncons walk is bounded by what
each step allocates (a `Some`, two pairs, a `()` reference, the rest of the text) and the
`match` on it; the number round trip saves two call-outs and two `String`s per step.

In the runtime the rope swap itself (same day) read: appending `"hi"` 10000 times 1.51 ms /
556 µs before, 1.06 ms / 311 µs after (interpreter / JIT); `Text.drop 1` 100000 times
11.9 ms / 3.56 ms before, 9.50 ms / 2.82 ms after. In `suite`, `Text.split` 0.87×, and
`Json.toText` 1.2× slower (6.1 µs to 7.4 µs per document, interpreter and JIT alike), which
was chased without result: every part (`literalForm`, `Text.join`, wrapping a text in
brackets, walking by `uncons`) is as fast or faster on the new rope, yet whole documents are
15 to 20% slower whatever their shape, and the threshold (32 or 64) makes no difference.
One oddity is left: `"[" ++ x ++ "]"` on a 50-character `x` takes 144 ns in the interpreter
against 48 ns on the old rope, while the JIT's C helpers do it in 51 ns. Paul's view: the
library should build its result as a single chunk through a builder anyway.
