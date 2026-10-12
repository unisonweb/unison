# 2026-10-11: the Haskell replacements under the JIT

The runtime replaces 25 base definitions and the `Map` type with Haskell at load time
(internals, "The Haskell replacements"). Under the JIT each replaced function is a call-out,
so each was measured against the Unison code it replaces, compiled, to decide which stay.
`unison-src/transcripts-manual/jit-replacements.md` on `jit_codebase` (base 7.19.2, json
1.3.4), optimized build, one run per column; `UNISON_JIT_STATS=1` runs confirmed which
call-outs happen. Every row does 100000 operations, or builds, sorts or walks 100000 elements
in all, except the JSON rows, which say their counts; the numbers are per operation or per
element. `Avro.decodeBinary` is not measured: the codebase has no Avro library.

The columns: the interpreter with every replacement (today's default with the JIT off);
`UNISON_JIT=on UNISON_REPLACEMENTS=on`, every replacement as a call-out from native code;
`UNISON_JIT=on UNISON_REPLACEMENTS=off`, base's Unison code for everything; and
`UNISON_JIT=on` alone, the tags this file decided (the `Map` family and `List.range` native,
the rest replaced). The last column is the third over the second: below 1, native code wins.

| row | interpreter | JIT, all replaced | JIT, none replaced | JIT, tagged (the default) | none / all |
| --- | --- | --- | --- | --- | --- |
| Map.insert: 10 Nat keys | 226 ns | 156 ns | 59 ns | 58 ns | 0.38 |
| Map.lookup: 100000 lookups in 10 Nat keys | 237 ns | 123 ns | 13 ns | 13 ns | 0.10 |
| Map.insert: 10 Text keys | 288 ns | 225 ns | 139 ns | 140 ns | 0.62 |
| Map.lookup: 100000 lookups in 10 Text keys | 303 ns | 196 ns | 108 ns | 108 ns | 0.55 |
| Map.fromList: 10 sorted pairs | 37 ns | 36 ns | 78 ns | 79 ns | 2.16 |
| Map.fromList: 10 scrambled pairs | 66 ns | 65 ns | 92 ns | 92 ns | 1.42 |
| Map.union: two maps of 10 keys, half overlapping | 571 ns | 647 ns | 205 ns | 206 ns | 0.32 |
| Map.intersect: two maps of 10 keys, half overlapping | 30 ns | 29 ns | 216 ns | 217 ns | 7.38 |
| Map.toList: 10 keys | 66 ns | 66 ns | 47 ns | 46 ns | 0.71 |
| Map.==: two equal maps of 10 keys | 41 ns | 41 ns | 1197 ns | 297 ns | 28.96 |
| Set.fromList: 10 scrambled Nats | 69 ns | 68 ns | 138 ns | 138 ns | 2.02 |
| Set.union: two sets of 10, half overlapping | 581 ns | 653 ns | 210 ns | 208 ns | 0.32 |
| Set.intersect: two sets of 10, half overlapping | 30 ns | 30 ns | 219 ns | 222 ns | 7.27 |
| Set.toList: 10 elements | 31 ns | 30 ns | 51 ns | 51 ns | 1.70 |
| Multimap.fromList: 10 pairs over 1 keys | 58 ns | 59 ns | 56 ns | 57 ns | 0.95 |
| List.range: 0 to 10 | 27 ns | 27 ns | 15 ns | 16 ns | 0.56 |
| List.sort: 10 sorted Nats | 32 ns | 31 ns | 176 ns | 31 ns | 5.63 |
| List.sort: 10 scrambled Nats | 41 ns | 41 ns | 209 ns | 41 ns | 5.11 |
| List.sort: 10 scrambled Texts | 55 ns | 54 ns | 232 ns | 54 ns | 4.28 |
| Map.insert: 1000 Nat keys | 333 ns | 256 ns | 202 ns | 208 ns | 0.79 |
| Map.lookup: 100000 lookups in 1000 Nat keys | 305 ns | 185 ns | 52 ns | 51 ns | 0.28 |
| Map.insert: 1000 Text keys | 521 ns | 426 ns | 502 ns | 504 ns | 1.18 |
| Map.lookup: 100000 lookups in 1000 Text keys | 478 ns | 352 ns | 283 ns | 280 ns | 0.80 |
| Map.fromList: 1000 sorted pairs | 29 ns | 32 ns | 85 ns | 85 ns | 2.68 |
| Map.fromList: 1000 scrambled pairs | 172 ns | 178 ns | 248 ns | 248 ns | 1.39 |
| Map.union: two maps of 1000 keys, half overlapping | 677 ns | 750 ns | 174 ns | 176 ns | 0.23 |
| Map.intersect: two maps of 1000 keys, half overlapping | 15 ns | 15 ns | 179 ns | 178 ns | 11.89 |
| Map.toList: 1000 keys | 58 ns | 63 ns | 28 ns | 29 ns | 0.45 |
| Map.==: two equal maps of 1000 keys | 29 ns | 29 ns | 3026 ns | 278 ns | 105.75 |
| Set.fromList: 1000 scrambled Nats | 172 ns | 177 ns | 308 ns | 308 ns | 1.74 |
| Set.union: two sets of 1000, half overlapping | 678 ns | 754 ns | 174 ns | 174 ns | 0.23 |
| Set.intersect: two sets of 1000, half overlapping | 15 ns | 15 ns | 178 ns | 178 ns | 11.92 |
| Set.toList: 1000 elements | 20 ns | 20 ns | 22 ns | 22 ns | 1.09 |
| Multimap.fromList: 1000 pairs over 100 keys | 143 ns | 150 ns | 185 ns | 186 ns | 1.23 |
| List.range: 0 to 1000 | 13 ns | 13 ns | 17 ns | 17 ns | 1.25 |
| List.sort: 1000 sorted Nats | 17 ns | 17 ns | 334 ns | 17 ns | 19.67 |
| List.sort: 1000 scrambled Nats | 124 ns | 126 ns | 559 ns | 126 ns | 4.44 |
| List.sort: 1000 scrambled Texts | 261 ns | 265 ns | 739 ns | 262 ns | 2.79 |
| Map.insert: 100000 Nat keys | 748 ns | 655 ns | 1806 ns | 1859 ns | 2.76 |
| Map.lookup: 100000 lookups in 100000 Nat keys | 450 ns | 294 ns | 406 ns | 432 ns | 1.38 |
| Map.insert: 100000 Text keys | 1530 ns | 1412 ns | 3047 ns | 3037 ns | 2.16 |
| Map.lookup: 100000 lookups in 100000 Text keys | 1085 ns | 888 ns | 1207 ns | 1241 ns | 1.36 |
| Map.fromList: 100000 sorted pairs | 164 ns | 164 ns | 252 ns | 253 ns | 1.54 |
| Map.fromList: 100000 scrambled pairs | 595 ns | 599 ns | 1903 ns | 1898 ns | 3.18 |
| Map.union: two maps of 100000 keys, half overlapping | 595 ns | 688 ns | 270 ns | 276 ns | 0.39 |
| Map.intersect: two maps of 100000 keys, half overlapping | 15 ns | 15 ns | 282 ns | 276 ns | 19.16 |
| Map.toList: 100000 keys | 197 ns | 206 ns | 167 ns | 172 ns | 0.81 |
| Map.==: two equal maps of 100000 keys | 60 ns | 62 ns | 4567 ns | 683 ns | 73.62 |
| Set.fromList: 100000 scrambled Nats | 560 ns | 565 ns | 2037 ns | 2304 ns | 3.61 |
| Set.union: two sets of 100000, half overlapping | 614 ns | 689 ns | 274 ns | 279 ns | 0.40 |
| Set.intersect: two sets of 100000, half overlapping | 14 ns | 15 ns | 279 ns | 279 ns | 19.00 |
| Set.toList: 100000 elements | 45 ns | 50 ns | 60 ns | 61 ns | 1.20 |
| Multimap.fromList: 100000 pairs over 10000 keys | 623 ns | 631 ns | 1213 ns | 1114 ns | 1.92 |
| List.range: 0 to 100000 | 68 ns | 69 ns | 16 ns | 16 ns | 0.23 |
| List.sort: 100000 sorted Nats | 51 ns | 61 ns | 672 ns | 61 ns | 10.95 |
| List.sort: 100000 scrambled Nats | 491 ns | 494 ns | 1238 ns | 496 ns | 2.51 |
| List.sort: 100000 scrambled Texts | 1076 ns | 1070 ns | 1823 ns | 1048 ns | 1.70 |
| Json.toText: a 4-field object, 10000 times (111 characters) | 1420 ns | 1257 ns | 18.3 µs | 1264 ns | 14.57 |
| Json.toText: an array of 10000 objects, twice (1208891 characters) | 53.63 ms | 45.55 ms | 233.13 ms | 45.55 ms | 5.12 |
| Json.unconsText: the 4-field object, 10000 times | 2005 ns | 2010 ns | 34.3 µs | 2000 ns | 17.08 |
| Json.unconsText: the array of 10000 objects, twice | 74.29 ms | 73.52 ms | 437.23 ms | 77.39 ms | 5.95 |
| Json.tryUnconsText: the 4-field object, 10000 times | 2009 ns | 2019 ns | 34.4 µs | 2038 ns | 17.04 |
| Json.tryUnconsText: the array of 10000 objects, twice | 73.82 ms | 72.97 ms | 434.10 ms | 73.14 ms | 5.95 |
| Bytes.read: 100000 reads | 110 ns | 7 ns | 10 ns | 7 ns | 1.49 |
| Bytes.read16le: 100000 reads | 121 ns | 8 ns | 33 ns | 8 ns | 3.94 |
| Bytes.read16be: 100000 reads | 121 ns | 8 ns | 33 ns | 8 ns | 3.95 |
| Bytes.read32le: 100000 reads | 124 ns | 12 ns | 36 ns | 12 ns | 3.10 |
| Bytes.read32be: 100000 reads | 122 ns | 12 ns | 36 ns | 12 ns | 3.10 |
| Bytes.read64le: 100000 reads | 120 ns | 18 ns | 42 ns | 18 ns | 2.33 |
| Bytes.read64be: 100000 reads | 120 ns | 18 ns | 41 ns | 18 ns | 2.30 |

## Decisions

| replacement | tag | why |
| --- | --- | --- |
| the `Map` type with `Map_insert`, `Map_lookup`, `Map_fromList`, `Map_union`, `Map_intersect`, `Map_toList`, `Map_eq`, `Set_fromList`, `Set_union`, `Set_intersect`, `Set_toList`, `Multimap_fromList` | `InterpreterOnly` | one tag for the family, since the functions take and return the type. Insert and lookup, what a program does most with a map, are 2.5 to 9× faster native at 10 and 1000 Nat keys and 1.2 to 2× at 10 and 1000 Text keys; `union` 3×, `toList` 1.2 to 2×. The family loses `fromList` (1.5 to 3×), `intersect` (7 to 19×), `==` (7 to 11× with `List.sort` still replaced), `Set.fromList` (2 to 4×), and everything at 100000 keys (1.4 to 3×). Those losses are not the generated code: see below. `UNISON_REPLACEMENTS=on` brings the Haskell map back for a program that needs it. |
| `List_range` | `InterpreterOnly` | native 1.7× faster at 10 and 4× at 100000; even at 1000 |
| `List_sort` | `Always` | the call-out is 2.5 to 20× faster at every size: base's `sortWith` is a merge sort calling `Universal.lteq` as a closure per comparison, and on anything but numbers, texts and bytes the comparison itself exits (below) |
| `Json_toText`, `Json_unconsText`, `Json_tryUnconsText` | `Always` | the call-out is 5 to 17× faster on both a 111-character object and a 1.2 MB array; the Unison serializer and parser are far from the Haskell ones |
| `Avro_decodeBinary` | `Always` | not measured; a Haskell decoder like the JSON one, over a 1 MB-scale input, kept on the same reasoning |
| `Bytes_read`, `Bytes_read16le`, `Bytes_read16be`, `Bytes_read32le`, `Bytes_read32be`, `Bytes_read64le`, `Bytes_read64be` | `Always` | the generator has native code for these foreign functions (`bytesForeign`), so the replacement costs 7 to 18 ns against 10 to 42 ns for base's Unison code (`decodeNat16le (Bytes.drop n b)`, a rope drop per read) |

## What the numbers say

- **The call-out column barely differs from the interpreter.** Every row whose work is in
  Haskell runs at the same speed from native code as from the interpreter, and the rows with a
  loop around the call-out (`Map.insert`, `Map.lookup`) gain only the loop. The stats run
  shows why there isn't more: the exit rule leaves the small functions whose body is a
  call-out interpreted (63 of them), so the code around a replaced function is interpreted
  too; the hottest exit of the run was a resume into one of them, 217 million times.
- **Where native code loses the `Map` family, and why.** At 100000 keys the native tree is
  1.4 to 3× slower on every operation: a `Bin` node is a `DataG`, three heap objects of about
  16 words against the Haskell node's 6, so the map is 2.6× the size and out of cache
  ([optimization-ideas, "Flat `GDataG`"](../optimization-ideas.md)). At small sizes the losses
  are base's algorithms against `Data.Map`'s: `fromList` is repeated insertion (78 ns per
  pair at 10 keys against 59 for an insert), where the Haskell one detects sorted input and
  builds in linear time; `intersect` walks one map looking each key up in the other (216 ns
  per key) where the Haskell hedge intersection shares subtrees (15 to 30 ns); `Map.==` sorts
  the two `toList`s and compares them with `===`, and comparing a pair of tuples is an exit
  (next point). These improve in base or with helpers over `DataG` nodes, not in the code the
  JIT generates for the current definitions.
- **Universal comparison of structured values exits.** The all-native run's hottest exit
  site was `Universal.<=` leaving at its `LEQU`, 78 million times, with every caller unwinding
  behind it: the generator compares numbers inline and texts and bytes through a helper, and
  anything else goes to the interpreter. This is what makes `Map.==` 1.2 to 4.6 µs per key
  natively and sorting pairs slow; an entry in optimization-ideas.md ("Universal comparison of
  structured values").
- **`Map.union` and `Set.union` are not replaced in this base.** The call-out column is slower
  than the interpreter's on them and the native one 3× faster, so the hash in
  `functionReplacementList` isn't base 7.19.2's `Map.union`; what runs is base's
  `Map.unionWith const`, whose inner inserts are call-outs when the family is replaced.
- **The `Bytes.read*` replacements cost nothing under the JIT**, since the foreign functions
  they introduce are among the ones the generator has native code for; without them base's
  Unison code (a rope `drop` and a decode per read) is 1.5 to 4× slower. The interpreter runs
  either at about 120 ns.

## The decision rule

Native code wins at the sizes a program commonly uses, the replacement goes
(`InterpreterOnly`); the call-out wins there, it stays (`Always`). Paul, 2026-10-11: "If JIT
wins at smallish sizes then I would not do the replacement. And for larger sizes we can
improve the JIT." The `Map` family's large-size loss is the `DataG` representation, listed in
optimization-ideas.md as the next step for it.
