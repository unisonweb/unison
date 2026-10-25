# `Unison.Util.Deque` against `Data.Sequence`

The strict finger tree that is the runtime's list since 2026-10-02, measured against
`Data.Sequence` (the list it replaced via a short-lived intermediate structure) with the
`deque` benchmark in `lib/unison-util-rope`
(`stack build --work-dir .stack-work-opt --flag unison-runtime:jit --bench unison-util-rope`,
about 6 minutes; `--ba "--csv FILE"` for the numbers). Optimized build. The table is the time
per operation as a ratio to `Data.Sequence`: below 1, the Deque is faster.

| Operation | n = 10 | n = 100 | n = 10,000 | n = 1,000,000 |
| --- | --- | --- | --- | --- |
| snoc (cons is the same) | 0.80 | 0.92 | 0.73 | 0.65 |
| uncons | 0.97 | 0.51 | 0.46 | 0.47 |
| unsnoc | 0.40 | 0.51 | 0.46 | 0.45 |
| queue at a steady size | 0.78 | 0.47 | 0.43 | 0.62 |
| lookup | 1.03 | 0.46 | 0.33 | 0.30 |
| take | 0.78 | 0.70 | 0.97 | 1.05 |
| drop | 1.04 | 1.13 | 1.23 | 1.33 |
| append of two pieces | 1.50 | 1.56 | 1.84 | 1.74 |
| append of 1 to 4 elements | 1.16 | 1.03 | 0.95 | 0.94 |
| `foldl'` | 0.45 | 0.38 | 0.43 | 0.44 |
| toList | 0.81 | 0.84 | 0.87 | 1.29 |
| `==` | 1.27 | 0.79 | 0.77 | 0.84 |
| fromList | 2.6 | 3.4 | 3.2 | 0.90 |

At 10,000 elements: snoc 7.5 ns, uncons 5.3 ns, lookup 32 ns, append 172 ns
(`Data.Sequence`: 10.3, 11.5, 96, 95; the old Deque: 14.8, 10.2, 50, 384). Left for later:
`append` of two large pieces, `drop`, and `fromList` of a long list (see the ideas).

What was tried and lost while it was built: array digits below the top level (pushes and pops
about 1 ns slower, append only 15% faster; half of append's time was then inside
`copySmallArray#`, `memmove` and `newSmallArray#`, which are calls into the runtime even for
three elements), and a single array-only node constructor in place of the inline node of
eight leaves (pushes 7% slower, pops 20 to 45%, `sum` and `toList` 60 to 70%). The structure
it replaced (worst-case O(1) pushes and pops, about twice the code, slower on every operation
measured) is in the branch's history. What is still left on the table is in the
[ideas](../optimization-ideas.md), "The list structure itself".
