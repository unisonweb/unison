# 2026-10-21: match dispatch from the type's arities

A data match (`genDMatchClosure` in Codegen.hs) switched on the pointer tag over every
data shape (`GEnum`, `GData1`, `GData2`, `GData5` have their own tags; `GData3`,
`GData4`, `GData6` and `GDataG` share tag 7 and were told apart by a switch on the info
pointer), one block per shape computing where that shape keeps the packed tag, a phi, a
load of the packed tag, and a switch on the constructor number. For

```unison
type MyTree = MyLeaf | MyNode MyTree Nat MyTree

sumTree t = match t with
  MyLeaf -> 0
  MyNode l v r -> sumTree l + v + sumTree r
```

that was, after LLVM, a five-way switch on the tag bits, a four-way switch on the info
pointer, eight shape blocks, a phi, the tag load and a two-way switch: for a `MyNode`
four dependent branches and two loads before its arm, most of it for shapes the type
has no constructor of.

Now the generator uses the constructor arities it has from the type's declaration
(`envTypes`): the arity fixes the shape, so the switch covers only the shapes the type
has a constructor of, a shape with one constructor branches straight to its arm, and
only a shape two or more constructors share (`Either`'s two `GData1`s) loads the packed
tag, switching among those. Any other shape or tag exits, the one compare being the
insurance against a runtime bug. The same match is now

```llvm
  switch i3 %trunc.i, label %exit [ i3 3, label %leaf   i3 -1, label %shape7 ]
shape7:
  %info = load i64, ptr %base.p
  %cond = icmp eq i64 %info, <GData3's info pointer>
  br i1 %cond, label %node, label %exit
```

two branches and one load for a `MyNode`, none for a `MyLeaf`. A request match
(`genRMatch`) is unchanged: it needs the packed tag for the ability number. The
interpreter is unchanged: `dataBranch` is one `case` and one compare already.

## The suite

`jit-benchmarks.md`, optimized build, `--jit on`, two runs against the run of
[2026-10-20 Name](2026-10-20-name.md). The tree's `Node` has four fields, a `GData4`
under tag 7, so it took the long way; lookups are 2.4 times faster and inserts 12%.
`Cons` was already one of the own-tag shapes and gains nothing measurable; the list walk
is bound by the list view helper. The records rows read a record, not a data closure.
Everything else is within noise (the records row's 8.2 ms in run 1 is 7.5 in run 2 and
was 7.6 before).

| before | run 1 | run 2 | run 2 / before | Benchmark |
| --- | --- | --- | --- | --- |
| 319.521µs | 316.803µs | 317.591µs | 0.99× | Sum 0 to 1 million |
| 35.637µs | 35.35µs | 35.375µs | 0.99× | fib 20 |
| 13.509µs | 13.945µs | 13.412µs | 0.99× | Cons list: map with a lambda (1000 elements) |
| 4.847µs | 4.821µs | 4.845µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 101.688µs | 89.379µs | 89.523µs | 0.88× | Binary tree: 1000 inserts |
| 38.392µs | 15.933µs | 16.18µs | 0.42× | Binary tree: 1000 lookups |
| 57.699µs | 57.714µs | 57.622µs | 1.00× | Apply a function argument 10000 times |
| 42.471µs | 41.463µs | 41.453µs | 0.98× | Mutate a Ref 10000 times |
| 66.448µs | 67.993µs | 70.103µs | 1.06× | Calls across definitions: Collatz steps for 1 to 1000 |
| 118.166µs | 118.403µs | 117.869µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 203.215µs | 201.34µs | 203.574µs | 1.00× | Text: append "hi" 10000 times |
| 1.92856ms | 1.895032ms | 1.905517ms | 0.99× | Text: drop 1, 100000 times |
| 197.266µs | 195.876µs | 204.085µs | 1.03× | Bytes: append 2 bytes 10000 times |
| 1.826588ms | 1.831884ms | 1.812338ms | 0.99× | Bytes: drop 1, 100000 times |
| 2.155743ms | 2.245706ms | 2.246209ms | 1.04× | Bytes: at, 100000 times |
| 3.336453ms | 3.274028ms | 3.235325ms | 0.97× | Text: uncons walk over 100000 characters |
| 292.98µs | 293.969µs | 295.221µs | 1.01× | List: cons 10000 elements, then a pattern walk |
| 146.617µs | 144.476µs | 146.537µs | 1.00× | List: at, 10000 times |
| 713.187µs | 704.736µs | 719.368µs | 1.01× | Nat.toText and Nat.fromText, 10000 times |
| 1.004766ms | 1.004123ms | 1.00326ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 487.496µs | 487.838µs | 486.336µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 538.805µs | 539.57µs | 538.875µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 44.252µs | 44.976µs | 44.488µs | 1.01× | MutableArray: fill, freeze and sum 10000 elements |
| 127.087µs | 127.945µs | 129.049µs | 1.02× | Ref.cas loop, 10000 times |
| 278.127µs | 275.976µs | 281.176µs | 1.01× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.596328ms | 8.225731ms | 7.539168ms | 0.99× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.088912ms | 4.987534ms | 5.040006ms | 0.99× | Records: a 16-field literal and four getters, 100000 times |
| 2.652819ms | 2.664711ms | 2.695991ms | 1.02× | Counter: 100000 requests |
| 5.077006ms | 5.054665ms | 5.197637ms | 1.02× | Store: get and put, 100000 iterations |
| 4.250484ms | 4.222375ms | 4.216646ms | 0.99× | handle installed per iteration, 10000 iterations |
| 8.421665ms | 8.253509ms | 8.646934ms | 1.03× | Store whose put requests an outer Counter, 100000 iterations |
| 461.946µs | 447.118µs | 450.714µs | 0.98× | Bytes: decodeNat64be walk over 80000 bytes |

The IR for the dump program (316 modules, mostly base) is 0.4% smaller before and after
optimization: matches are a small part of the IR by bytes, and the shape blocks were
small.

## Map rows

Base's `Map` is `Tip` (a `GEnum`) and `Bin` with five fields (a `GData5`, one of the
own-tag shapes), so a match on it was already on the short path: the tag switch, the
shape block, the load of the packed tag and the constructor switch. Now it is one compare
of the tag bits. The broad suite's rows and one more, run on their own (optimized build,
each `on` line one run; the test matrix was running on other cores at the time, so these
carry some noise). The last column is the 2026-10-16 binary ([against
trunk](2026-10-16-against-trunk.md)), which is several changes older than this one, so
the difference is not all this change's; the interpreter column agrees with that day's
(196 against 197 ns, 269 against 299 ns).

| row | `off` | `on` | `on` | `on`, 2026-10-16 |
| --- | --- | --- | --- | --- |
| Map.fromList (range 0 1000) | 29.8 µs | 75.2 µs | 76.1 µs | 73.7 µs |
| Map.lookup (1k element map) | 196 ns | 25 ns | 26 ns | 31 ns |
| Map.insert (1k element map) | 269 ns | 103 ns | 102 ns | 163 ns |
| Map.lookup of all 1000 keys, summed | 290 µs | 69.2 µs | 69.0 µs | |

`Map.fromList` is the known story: the JIT turns the `Map` replacement off, so the
interpreter's row is Haskell's `Data.Map` and the native row is base's own tree.
