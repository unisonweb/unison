# The finger-tree rope against the old rope

The rope under `Text` and `Bytes` became the Deque's finger tree with chunks for elements on
2026-10-02, written as `Rope2` beside the size-balanced binary tree it replaced and
benchmarked against it with the `rope` benchmark in `lib/unison-util-rope`
(`stack bench --work-dir .stack-work-opt --flag unison-runtime:jit unison-util-rope:bench:rope`,
about 4 minutes). It goes through a record of the operations so that another implementation
can be put beside it. Optimized build, chunks of `Data.Text` as `Unison.Util.Text` makes them,
threshold 64. "Loaded" texts are in chunks of 512 characters, as `Text.fromText` makes them;
"built" ones were made by appending three characters at a time. Below 1, the new rope is faster.

| Operation | n = 20 | n = 1000 | n = 100,000 | n = 1,000,000 |
| --- | --- | --- | --- | --- |
| snoc one character at a time | 0.68 | 0.42 | 0.29 | 0.26 |
| cons one character at a time | 0.68 | 0.43 | 0.29 | 0.26 |
| append a 3-character text | 0.63 | 0.43 | 0.31 | 0.28 |
| append a 40-character text | 0.96 | 0.33 | 0.24 | 0.15 |
| `Text.uncons` to the end (loaded / built) | 0.64 | 0.59 / 0.46 | 0.48 / 0.42 | 0.46 / 0.39 |
| `Text.unsnoc` to the end | 0.77 | 1.00 | 0.81 | 0.72 |
| drop 10 to the end | 0.64 | 0.64 | 0.53 | 0.49 |
| index (loaded / built) | 1.03 | 1.10 / 0.84 | 1.02 / 0.81 | 1.01 / 0.83 |
| take (loaded / built) | 0.63 | 0.73 / 0.70 | 0.96 / 0.94 | 0.97 / 1.06 |
| drop (loaded / built) | 0.53 | 0.67 / 0.76 | 0.99 / 1.01 | 1.09 / 1.14 |
| take and drop near the ends | 0.56 | 0.63 | 0.57 | 0.46 |
| append two halves (loaded / built) | 0.69 | 0.40 / 2.05 | 6.8 / 8.8 | 14 / 11 |
| `==`, `compare` | 1.00 | 0.51 | 0.54 | 0.52 |
| uncons a chunk at a time | 0.92 | 0.11 | 0.06 | 0.05 |
| the list of chunks | 0.87 | 0.58 | 0.70 | 0.72 |

The one loss is appending two large texts to each other (see the ideas: the finger tree
packs the inner digits into nodes at every level, where the old rope made one node); it is
150 to 290 ns against 20 to 30, and a text built by appending short pieces never takes
that path. Thresholds 16, 32, 64 and 128 were tried: 16 doubles the cost of walking and
comparing, 128 makes appending 40-character pieces and indexing short-chunk texts slower,
and 64 is as fast as 32 at building and half the cost at walking and comparing.

In the runtime (`jitSuite`, interpreter / JIT): appending `"hi"` 10000 times 1.51 ms /
556 µs before, 1.06 ms / 311 µs after; `Text.drop 1` 100000 times 11.9 ms / 3.56 ms
before, 9.50 ms / 2.82 ms after. In `suite`, `Text.split` 0.87×, and `Json.toText` 1.2×
slower (6.1 µs to 7.4 µs per document, interpreter and JIT alike). Everything else within
3%. The `Json.toText` loss was chased with the pre-swap commit built in a worktree and a
transcript that times its parts: every part (`literalForm`, `Text.join`, wrapping a text
in brackets, walking by `uncons`) is as fast or faster on the new rope, and the Core of
`Unison.Util.Text` shows the rope operations specialised to `Chunk`, yet whole documents
are 15 to 20% slower whatever their shape (20 numbers, 20 strings, nested objects), and
the threshold (32 or 64) makes no difference. One oddity is left: `"[" ++ x ++ "]"` on a
50-character `x` takes 144 ns in the interpreter against 48 ns on the old rope, while the
JIT's C helpers do it in 51 ns and the Haskell benchmark in 26 + 26 ns. Paul's view
(2026-10-02): `Json.toText` should build its result as a single chunk through a builder
anyway, so this is the library's to fix, not the rope's. The append of two multi-chunk
texts was rewritten on the way (the seam is joined without rebuilding either side, and
the shorter side's digit is the one copied); it didn't move `Json.toText`, but made the
rope benchmark's appends 10 to 40% faster.
