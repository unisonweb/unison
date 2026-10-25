# Bytes as a closure constructor (2026-10-16)

A bytes value was `Foreign (WrapBytes rope)`: two closures before the rope, allocated for
every result, and two loads and two checks on every primitive that reaches it. Now it is
`BytesC rope` (`GBytes`), one object of two words, as texts and lists became the day before
([2026-10-15 text and lists](2026-10-15-text-list.md)). `GBytes` comes after the six tagged
constructors, so it has pointer tag 7 and a bytes primitive checks the tag and then the
info pointer, one load; a text primitive checks the tag alone. The two rope kinds in C are
now one shape, so `RopeKind` lost its `direct` flag and the two-level path.

Nothing on the wire changed (bytes are their own `BLit` case of the ANF `Value`), and
`Universal.compare` and the hash are unchanged: `closureNum` 3, two bytes values compare by
contents, a bytes value against another builtin kind inside `Any` raises as before.
`jit-tests.md` gained a stanza "Bytes as a closure constructor"; the 18 rows of the test
matrix pass on the fast build.

## Timings

Optimized build, `jit-benchmarks.md`, two rounds each; ms per iteration; before is
758172255 (bytes foreign), after is this change. The second round is the table; the first
round agrees within 2% on every row shown.

| row | interpreter, before | after | | native, before | after | |
| --- | --- | --- | --- | --- | --- | --- |
| Text: append "hi" 10000 times | 1.03 | 1.01 | -2% | 0.244 | 0.239 | -2% |
| Text: drop 1, 100000 times | 9.24 | 9.07 | -2% | 1.93 | 1.88 | -2% |
| Bytes: append 2 bytes 10000 times | 0.996 | 0.974 | -2% | 0.260 | 0.237 | -9% |
| Bytes: drop 1, 100000 times | 8.33 | 8.23 | -1% | 2.10 | 1.82 | -14% |
| Bytes: at, 100000 times | 10.3 | 10.4 | +1% | 2.21 | 2.15 | -3% |
| Text: uncons walk over 100000 characters | 10.4 | 10.4 | +0% | 3.40 | 3.36 | -1% |
| Bytes: decodeNat64be walk over 80000 bytes | 1.55 | 1.53 | -1% | 0.515 | 0.472 | -8% |

The bytes rows that allocate a result per step gain the most: `drop` 2.10 → 1.82 ms and
`append` 0.260 → 0.237 ms native, each result now one allocation of two words instead of
two of four, and each primitive one check instead of two. `Bytes.at` allocates no bytes
result and gains only the check (3%). The interpreter's bytes rows move within noise; the
text rows are the control and move within noise too. The other rows of the suite were not
expected to move and the logs show them within 2%.
