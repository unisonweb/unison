# 2026-10-15: constructor order

**What.** GHC gives each of a type's first six constructors its own pointer tag (1 to 6) and
the seventh and later tag 7, so a `case` on a value of a type with more than seven
constructors is a `switch` on the tag for the first six and, for tag 7, a load of the
constructor index from the info table and a second switch (checked on a 9-constructor type
with `-ddump-cmm`: `switch [1 .. 7] (R2 & 7)` then `I32[I64[R2 & -8] - 4]`; a match on one
late constructor skips the tag and reads the info table every time). The interpreter's
dispatch types had their constructors in whatever order they were written, so `Let` and
`DMatch` sections, `Lit` and `Pack` instructions and the `Nat` comparisons paid the info-table
read on every dispatch while `Jump`, `RefCAS`, `DLLCall` and `DECI` held cheap tags. This
run counted the dispatches, reordered the types by the counts, and measured.

**Counting.** A throwaway module (not committed) with a 512-slot `MutablePrimArray` and
`countCtor base x = write (base + dataToTag# x)`, called at the top of `eval` (sections), in
the `Ins` case (instructions, and the primitive of a `Prim1` or `Prim2`), at the top of
`dataBranch`, in `apply`, in `marshalUnwrapForeignIO` (the closure and the wrapper) and in
`compareVal` and `Eq Closure`; a thread wrote the array to the file named by
`UNISON_COUNTS` once a second. Optimized build, `jit-benchmarks.md` with `UNISON_JIT=off`
and `on` and `jit-tests.md` with `off`. The foreign primitives' own unwrapping
(`readAtIndex` in `Foreign/Function.hs`) wasn't hooked, so the wrapper counts only cover the
`Ref` path; the primitive counts say what the rest carries.

**Sections**, share of 17.4 G dispatches on the benchmark suite interpreted (85 M on the
tests; the JIT-on run, 138 M, is what the interpreter still sees under the JIT):

| constructor | was | benchmarks off | tests off | benchmarks on |
| --- | --- | --- | --- | --- |
| `Ins` | 6 | 61.4% | 59.2% | 51.3% |
| `DMatch` | 10 | 13.3% | 11.8% | 2.8% |
| `Call` | 2 | 11.0% | 19.4% | 22.3% |
| `Let` | 7 | 5.4% | 4.6% | 11.4% |
| `Yield` | 5 | 5.4% | 4.6% | 1.3% |
| `App` | 1 | 2.5% | 0.4% | 10.8% |
| `RMatch` | 12 | 1.0% | 0 | 0 |
| `Jump`, `Match`, `NMatch`, `Die`, `Exit` | 3, 4, 11, 8, 9 | 0 | 0 | 0 |

**Instructions**, share of 10.7 G:

| constructor | was | benchmarks off | tests off | benchmarks on |
| --- | --- | --- | --- | --- |
| `Prim2` | 2 | 48.3% | 54.9% | 10.6% |
| `Lit` | 15 | 30.8% | 37.0% | 5.9% |
| `Prim1` | 1 | 5.2% | 1.6% | 1.0% |
| `ForeignCall` | 4 | 5.1% | 0 | 1.0% |
| `Pack` | 11 | 4.1% | 6.1% | 1.4% |
| `Name` | 9 | 1.9% | 0 | 59.2% |
| `InLocal`, `SetAff` | 18, 6 | 1.5% each | 0 | 0.4% each |
| `RecUnpack`, `RefCAS`, `RecPack` | 13, 3, 12 | 0.7%, 0.5%, 0.3% | 0 | 0 |
| `Reset` | 17 | 0 | 0 | 19.6% |
| `DLLCall` | 5 | 0 | 0 | 0 |

**`Prim2`**, share of 5.16 G: `ADDN` 46.5% (was 2), `EQLN` 19.8% (18), `LESN` 8.0% (24),
`DRPN` 5.2% (45), then the rows' own operations at 2.7% each (`IDXB`, `DRPB`, `DRPT`,
`ADDF`), `MODN` 1.8% (10), `MULN` 1.7% (6), `CATB`, `CATT`, `REFW`, `DIVN` at 1.1%. The
tests: `ADDN` 52.9%, `EQLN` 19.8%, `DRPN` 16.5%, `LESN` 4.7%, `SNOC` 1.9%, `MULN` 1.4%.
`ADDI`, `SUBI`, `SUBN`, `MULI`, `DIVI`, held the cheap tags with a few hundred dispatches:
`n + 1` and `n - 1` on `Nat` compile to `ADDN` and `DRPN`, never `INCN` or `DECN`.

**`Prim1`**, 553 M on the benchmarks, 826 K on the tests: the benchmark rows' own operations
(the float functions, `ITOF`, `REFR`, `TIKR`, `RRFC`, `UCNS`) and, on the tests, `VWLS`
55.8%, `VWRS` 19.3%, `SIZS` 8.7%, `SIZT` 4.7%, `PAKB` 4.1%. `DECI`, `DECN`, `INCI`, `INCN`,
`NEGI` and `SGNI` held the six cheap tags and were dispatched 204 times in all.

**Closures.** At `dataBranch` (2.31 G): `Enum` 68.6%, `Data2` 15.4%, `Data1` 10.7%, `Data4`
5.3%, nothing else; at `apply` (445 M): `PAp` only; `marshalUnwrapForeignIO` (111 M):
`Foreign` only, `WrapIORef` and `WrapTicket` (the `Ref` rows). `GClosure`'s order (`PAp`,
`Enum`, `Data1`, `Data2`, `Data3`, `Data4`) is already right at the two hot sites, so it
wasn't changed; `Data3` was never matched and `Foreign` is matched by every text, bytes,
list and map primitive, which is "`Foreign` among the first six closure constructors" in
optimization-ideas.md.

**The new orders** (`MCode.hs`, `Stack.hs`; nothing serializes or orders by them: the
`Tag` instances are explicit, `compareClosure` orders kinds by `closureNum`,
`compareForeign` never orders two kinds):

- sections: `Ins`, `Call`, `DMatch`, `Let`, `Yield`, `App`, then the rest;
- instructions: `Prim2`, `Lit`, `Prim1`, `ForeignCall`, `Pack`, `Name`;
- `Prim2`: `ADDN`, `EQLN`, `LESN`, `DRPN`, `LEQN`, `MULN` (the last two by judgment: `<=`
  and `*` are the next `Nat` operations in ordinary code; `MODN` measured slightly above
  `MULN` here);
- `Prim1`: `VWLS`, `VWRS`, `SIZS`, `SIZT`, `UCNS`, `REFR` (list views and sizes, text
  size and uncons, `Ref.read`);
- `Foreign`: `WrapText`, `WrapSeq`, `WrapBytes`, `WrapMap`, `WrapArray`, `WrapNatural` (by
  judgment from the primitive counts; `WrapIORef` and `WrapMutableArray` stay at tag 7
  because `probeLayouts` asks for them by that tag).

**Timings.** Optimized build, the suite twice per binary and mode alternating, best of the
two per row, in ms; before is 598613b31 (the one-dispatch matcher), after is the reorder.
`jit-tests.md` identical in all three modes with the JIT confirmed on.

| row | interpreter before | interpreter after | | native before | native after | |
| --- | --- | --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 67.5 | 64.4 | -5% | 0.331 | 0.323 | -2% |
| fib 20 | 1.45 | 1.21 | -16% | 0.0359 | 0.0357 | -0% |
| Cons list: map with a lambda (1000 elements) | 0.0844 | 0.0853 | +1% | 0.0132 | 0.013 | -2% |
| Cons list: foldLeft with a lambda (1000 elements) | 0.0852 | 0.0865 | +2% | 0.00491 | 0.00485 | -1% |
| Binary tree: 1000 inserts | 1.35 | 1.2 | -11% | 0.114 | 0.109 | -5% |
| Binary tree: 1000 lookups | 0.729 | 0.623 | -15% | 0.0446 | 0.0438 | -2% |
| Apply a function argument 10000 times | 1.16 | 1.07 | -8% | 0.0578 | 0.0573 | -1% |
| Mutate a Ref 10000 times | 0.848 | 0.819 | -3% | 0.0441 | 0.0424 | -4% |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.99 | 7.22 | -10% | 0.0671 | 0.0683 | +2% |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 16.2 | 15.3 | -6% | 0.12 | 0.121 | +1% |
| Text: append "hi" 10000 times | 1.09 | 1.04 | -4% | 0.269 | 0.268 | -0% |
| Text: drop 1, 100000 times | 9.83 | 9.43 | -4% | 2.13 | 2.07 | -3% |
| Bytes: append 2 bytes 10000 times | 1.05 | 1.01 | -4% | 0.269 | 0.263 | -2% |
| Bytes: drop 1, 100000 times | 8.89 | 8.56 | -4% | 2.12 | 2.07 | -2% |
| Bytes: at, 100000 times | 10.2 | 10.5 | +2% | 2.23 | 2.19 | -1% |
| Text: uncons walk over 100000 characters | 11.1 | 10.9 | -1% | 3.57 | 3.63 | +1% |
| Nat.toText and Nat.fromText, 10000 times | 8.16 | 7.92 | -3% | 0.725 | 0.727 | +0% |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.8 | 30.9 | +1% | 1 | 1 | -0% |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 64.6 | 66.1 | +2% | 0.49 | 0.488 | -0% |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 29.3 | 29.8 | +2% | 0.547 | 0.542 | -1% |
| MutableArray: fill, freeze and sum 10000 elements | 3.98 | 4.01 | +1% | 0.0493 | 0.0461 | -7% |
| Ref.cas loop, 10000 times | 1.07 | 1.06 | -1% | 0.133 | 0.131 | -2% |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.45 | 7.33 | -2% | 0.296 | 0.357 | +21% |
| Records: a literal, an update, a pattern and two getters, 100000 times | 97.4 | 93.4 | -4% | 7.59 | 7.66 | +1% |
| Records: a 16-field literal and four getters, 100000 times | 76.1 | 73.8 | -3% | 5.25 | 5.08 | -3% |
| Counter: 100000 requests | 16.8 | 15.4 | -9% | 3.51 | 3.31 | -6% |
| Store: get and put, 100000 iterations | 27.8 | 26.1 | -6% | 6.69 | 6.52 | -2% |
| handle installed per iteration, 10000 iterations | 6.36 | 6.09 | -4% | 4.34 | 4.29 | -1% |
| Store whose put requests an outer Counter, 100000 iterations | 39.7 | 38.1 | -4% | 10.7 | 10.6 | -1% |
| Bytes: decodeNat64be walk over 80000 bytes | 1.58 | 1.53 | -3% | 0.504 | 0.491 | -2% |

The interpreter rows that dispatch most are 5 to 16% faster (`fib` 1.45 → 1.21 ms, tree
lookups 0.729 → 0.623, Collatz 7.99 → 7.22, Counter 16.8 → 15.4); the rows dominated by a
foreign call or a C helper move within noise. Native rows are unchanged, as expected: native
code dispatches on none of these types, and the interpreter residue under the JIT is small.
The hash row's native reading is the row's bimodal noise, not the change: the baseline read
0.296 and 0.351 ms in its two rounds, and the standalone loop (`hashbench/h.md`, three runs
each, alternating) has the same floor on both binaries, 255 µs per 10000 hashes.

## `PAp` without its own tag

Paul's follow-up: a partial application is only scrutinized by a call, whose other work (the
argument moves, the segment dump, the arity check, the native cell read) dwarfs an info-table
read, so `GPAp` needn't hold one of the six tags. `GPAp` moved after `GDataG`: `Enum`,
`Data1` … `Data5` hold tags 1 to 6 and `PAp` shares 7, the native closure call compares the
tag and then the info pointer (`closureCalleeFrom`), and the C side takes the `Data1`,
`Data2` and `PAp` tags from the inits instead of literals (the first attempt left the
literals in place, the list self-check turned the JIT off, and that round's native column
was the interpreter; development.md has the lesson).

The closure-call loop (`applyn/a.md`, 10000 calls per reading, three runs each,
alternating): tag 1 best 55 to 57 µs, tag 7 best 57 to 58 µs, so under 0.1 ns per call, inside
the spread. The suite, optimized build, `jit-tests.md` identical in all three modes with the
JIT confirmed on; native best of two rounds, interpreter one round:

| row | interpreter, PAp tag 1 | PAp tag 7 | | native, PAp tag 1 | PAp tag 7 | |
| --- | --- | --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 63.7 | 63.7 | -0% | 0.319 | 0.319 | +0% |
| fib 20 | 1.18 | 1.2 | +1% | 0.0353 | 0.0354 | +0% |
| Cons list: map with a lambda (1000 elements) | 0.0841 | 0.0848 | +1% | 0.013 | 0.0135 | +4% |
| Cons list: foldLeft with a lambda (1000 elements) | 0.0854 | 0.0849 | -1% | 0.00484 | 0.00484 | +0% |
| Binary tree: 1000 inserts | 1.19 | 1.2 | +1% | 0.102 | 0.101 | -1% |
| Binary tree: 1000 lookups | 0.606 | 0.598 | -1% | 0.0436 | 0.0332 | -24% |
| Apply a function argument 10000 times | 1.06 | 1.06 | -0% | 0.057 | 0.0577 | +1% |
| Mutate a Ref 10000 times | 0.806 | 0.806 | +0% | 0.0415 | 0.042 | +1% |
| Calls across definitions: Collatz steps for 1 to 1000 | 6.98 | 6.94 | -1% | 0.0663 | 0.0678 | +2% |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 15.2 | 15.2 | -0% | 0.118 | 0.118 | +0% |
| Text: append "hi" 10000 times | 1.04 | 1.02 | -2% | 0.263 | 0.266 | +1% |
| Text: drop 1, 100000 times | 9.18 | 9.18 | +0% | 2.06 | 2.06 | -0% |
| Bytes: append 2 bytes 10000 times | 0.991 | 0.985 | -1% | 0.26 | 0.262 | +1% |
| Bytes: drop 1, 100000 times | 8.41 | 8.39 | -0% | 2.06 | 2.06 | -0% |
| Bytes: at, 100000 times | 10.4 | 10.4 | +1% | 2.19 | 2.21 | +1% |
| Text: uncons walk over 100000 characters | 10.7 | 10.6 | -1% | 3.57 | 3.58 | +0% |
| Nat.toText and Nat.fromText, 10000 times | 7.84 | 8.33 | +6% | 0.718 | 0.722 | +1% |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.2 | 32.4 | +7% | 1 | 1 | +0% |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 64.7 | 64.8 | +0% | 0.487 | 0.486 | -0% |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 29.4 | 30.6 | +4% | 0.54 | 0.539 | -0% |
| MutableArray: fill, freeze and sum 10000 elements | 3.85 | 3.96 | +3% | 0.0459 | 0.046 | +0% |
| Ref.cas loop, 10000 times | 1.08 | 1.05 | -3% | 0.129 | 0.13 | +1% |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.4 | 7.35 | -1% | 0.358 | 0.284 | -21% |
| Records: a literal, an update, a pattern and two getters, 100000 times | 93.2 | 93.2 | -0% | 7.47 | 7.89 | +6% |
| Records: a 16-field literal and four getters, 100000 times | 72.7 | 72.4 | -0% | 5.07 | 5.08 | +0% |
| Counter: 100000 requests | 15.3 | 15.3 | +0% | 3.47 | 3.33 | -4% |
| Store: get and put, 100000 iterations | 26.3 | 26.3 | -0% | 6.59 | 6.62 | +0% |
| handle installed per iteration, 10000 iterations | 6.05 | 6.08 | +1% | 4.23 | 4.29 | +1% |
| Store whose put requests an outer Counter, 100000 iterations | 37.8 | 37.6 | -1% | 10.5 | 10.5 | -0% |
| Bytes: decodeNat64be walk over 80000 bytes | 1.53 | 1.53 | -0% | 0.49 | 0.496 | +1% |

The closure-call row is +1%. Four native rows moved consistently in both rounds although
none of them scrutinizes a `PAp` natively: tree lookups 0.044 → 0.034 ms and the hash row
0.358 → 0.285 ms faster, the records row 7.5 → 8.0 ms and the cons-list map 0.013 → 0.014 ms
slower. The data tags all shifted down by one, which changes the `switch` constants in every
match and the placement of everything after; the signs don't follow the dispatch counts,
so these are placement effects, like the closure-call row of the data-layout round, and are
recorded rather than explained. Kept: the tag `PAp` gave up is free for a text or list
constructor (optimization-ideas.md).
