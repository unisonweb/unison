# The `jit` branch against `trunk`: what the diff is made of

Measured on 2026-10-06 with `git diff --numstat` from the merge base with `trunk` (`a1652e3ad`, the
head of `trunk` at the time), after the M0 spike programs were deleted. Lines are added plus removed.

21,265 lines added, 349 removed, 21,614 in all. This writeup counts itself among the docs.

## By kind of file

| Kind | Files | Added | Removed | Lines | Share |
| --- | --- | --- | --- | --- | --- |
| New Haskell: JIT modules (`Runtime/JIT.hs`, `Runtime/JIT/*.hs`) | 12 | 7,601 | 0 | 7,601 | 35% |
| C (`cbits/jit_rt.c` 3,934, `jit_llvm.c` 168, `jit_rt.h` 79) | 3 | 4,181 | 0 | 4,181 | 19% |
| Markdown docs (`Runtime/JIT/*.md` and `Runtime/JIT/benchmarks/`) | 22 | 3,048 | 0 | 3,048 | 14% |
| Transcripts: tests 2,182, benchmarks 273 | 3 | 2,455 | 0 | 2,455 | 11% |
| New Haskell: `Deque`, `Skews`, rope tests and benchmarks | 7 | 2,446 | 0 | 2,446 | 11% |
| Existing Haskell, modified | 16 | 1,294 | 343 | 1,637 | 8% |
| Build config and CI | 8 | 244 | 6 | 250 | 1% |

The modified existing code is concentrated in three files:

| File | Added | Removed | What changed |
| --- | --- | --- | --- |
| `lib/unison-util-rope/src/Unison/Util/Rope.hs` | 595 | 196 | rebuilt on the `Deque` finger tree, with a layout C can read |
| `unison-runtime/src/Unison/Runtime/Machine.hs` | 290 | 84 | the trampoline: `runNative`, exit handling, counting |
| `unison-runtime/src/Unison/Runtime/MCode.hs` | 254 | 8 | the `NativeCell` type and its operations |
| `Stack.hs`, `Interface.hs`, `Machine/Types.hs`, `Foreign/Function.hs`, `ArgParse.hs` | 132 | 38 | frame records, startup, config, foreign-call hooks, flags |
| eight other files | 23 | 17 | one to six lines each |

## By role in the design

| Area | What it is | Where | Lines | Share |
| --- | --- | --- | --- | --- |
| Code generation | MCode to LLVM IR | `Codegen.hs` 4,140, `Strict.hs` 50 | 4,190 | 19% |
| Documentation | design, usage, development, ideas, builtins, this file, and the benchmark runs (internals.md was folded into design.md on 2026-10-16) | 22 Markdown files | 3,048 | 14% |
| Native builtins | C implementations of the list, Text, Bytes, array, Ref and hashing primitives, and the Haskell side that registers them | `jit_rt.c` sections Lists 1,067, Ropes 846, the rest of Text and Bytes 857, Arrays and Refs 290, murmurHash 251; `Native.hs` 363 | 3,674 | 17% |
| Runtime data structures | `Deque` and `Skews`, the `Rope` rework, their tests and benchmarks, small `Text` and `Bytes` changes | `lib/unison-util-rope`, `Util/Text.hs`, `Util/Bytes.hs` | 3,257 | 15% |
| Tests and benchmarks | the transcripts | `jit-tests.md`, `jit-benchmarks.md`, `jit-suite.md` | 2,455 | 11% |
| Trampoline and interpreter integration | native cells, exits, frames, the constant pool, entering and leaving native code, partial applications | `Exits.hs`, `Frames.hs`, `Pool.hs`, `MCode.hs`, `Machine.hs`, `Machine/Types.hs`, `Stack.hs`, `Interface.hs`, `Foreign/Function.hs`, `jit_rt.h`, `jit_rt.c` sections Entering native code 136 and Partial applications 217 | 1,702 | 8% |
| Compile driver | what to compile and when, budgets, configuration, the LLVM shim | `JIT.hs` 410, `Compile.hs` 478, `Config.hs` 188, `Estimate.hs` 237, `LLVM.hs` 126, `jit_llvm.c` 168 | 1,607 | 7% |
| Closure layout and allocation | probing GHC's heap layouts at startup, allocation and the write barrier | `Layout.hs` 1,210, `jit_rt.c` sections Allocation 129, Numbers 31, layout probe 33 | 1,403 | 6% |
| Build config, CI and CLI | packaging, command-line flags, the startup message | `package.yaml`, `.cabal`, `stack.yaml`, `hie.yaml`, `test.yaml`, `ArgParse.hs`, `Main.hs` | 282 | 1% |

The compiler proper (code generation, the driver, layout, the trampoline) is 8,900 lines, 41% of
the diff, and `Codegen.hs` alone is nearly half of that. The native builtins plus the data-structure
rework are almost as large, 6,900 lines or 32%: the cost of the M5 to M7 decision to give native
code direct access to lists, Text and Bytes. Docs and tests together are a quarter of the branch.

The JIT modules by size:

| Module | Lines |
| --- | --- |
| `Codegen.hs` | 4,140 |
| `Layout.hs` | 1,210 |
| `Compile.hs` | 478 |
| `JIT.hs` | 410 |
| `Native.hs` | 363 |
| `Estimate.hs` | 237 |
| `Config.hs` | 188 |
| `Pool.hs` | 168 |
| `Exits.hs` | 162 |
| `LLVM.hs` | 126 |
| `Frames.hs` | 69 |
| `Strict.hs` | 50 |

## Interpreter bugs found by the native code and fixed on both sides

Every native path is checked against the interpreter, so a disagreement is a bug in one of them;
twice it was the interpreter's.

- **The sized Int foreign conventions tagged their results Nat** (2026-10-11): the
  `ForeignConvention` instances for `Int8`, `Int16`, `Int32` and `Int64` wrote their results back
  with `pokeN`, so an `FFI.Ptr.Int8.get` returned a value whose type tag said Nat. Found by the
  pointer tests; now `pokeI`.
- **Universal ordering of floats around zero** (2026-10-11): `compareAsFloat` in `Stack.hs` clears
  the sign bit before comparing two non-positive floats backwards, but cleared bit 64, which on a
  64-bit `Int` is a no-op, so `Universal.compare 0.0 -1.0` was `-1`, every negative float ordered
  above `0.0`, and `-0.0` ordered above every negative float and below `0.0`. The native comparison
  reproduced it at first (outputs must match), then both were fixed to clear bit 63;
  `runtime-tests.md` had recorded the wrong answers for `compare +0.0 (-0.0)` and its mirror.

## Records (merged from trunk on 2026-10-15)

Trunk's structural records (`GRecord`, `RecPack`, `RecUnpack`, `RecSet`) were merged and made
native the same day: the three instructions are generated, and the comparison walkers and the
hash handle records. Two things in the interpreter changed with it: the elements of a record's
value vector are built evaluated (`recordVals`), since native code reads them as closures, and
a shape keeps the slot of each field in two unboxed sorted arrays searched by `shapeSlot`
rather than in an `EnumMap`, which both the interpreter and native code look up faster
(`benchmarks/2026-10-15-positions.md`). "Records" in design.md has the details.

## Flat data constructors (2026-10-15)

A constructor with three or more fields was a `GDataG` holding a `Seg` tuple, which can't be
unpacked: five objects and three loads per field. `GData3` to `GData6` are flat now, one
object with the fields inline in both halves and the reference last; `GDataG` (seven or
more), `GPAp` and `GCaptured` hold their two segment arrays as two fields with no boxes. The
pattern synonyms keep every construction and matching site as it was; `buildData`,
`dumpDataNoTag` and `dataBranch` handle the new shapes, and `dataBranch` is one generic
matcher now rather than three copies. The pointer-tag order changed: `Data5`, `Data6`,
`DataG` and `Captured` are told apart by info pointer. Measured in
`benchmarks/2026-10-15-data-layout.md`; "Layouts" in design.md has the layout.

## Constructor order (2026-10-15)

GHC tags pointers to a type's first six constructors and reads the info table for the rest,
and the interpreter's dispatch types were in declaration order: `Let` and `DMatch`, `Lit`
and `Pack`, and the `Nat` comparisons all paid the info-table read on every dispatch while
`Jump`, `RefCAS` and `DECI` held cheap tags. The dispatches were counted on both suites and
`GSection`, `GInstr`, `Prim1`, `Prim2` and `Foreign` reordered by the counts. `GClosure`
was already right at its match sites; `GPAp` then gave up its tag (only a call scrutinizes
it, and a call's other work dwarfs an info-table read: measured at under 0.1 ns per call),
so the data constructors with 0 to 5 fields hold tags 1 to 6 and a slot is free for a text
or list constructor later. The tags C writes into closures now come from the probes rather
than literals. No wire format or ordering depends on declaration order.
Counts and timings in `benchmarks/2026-10-15-constructor-order.md`.

## Texts and lists as closure constructors (2026-10-15)

A text was `Foreign (WrapText rope)` and a list `Foreign (WrapSeq deque)`: two objects of
two words each, two loads and two checks on every primitive, both allocated for every
result. They are `TextC rope` and `ListC deque` now, constructors of `GClosure` holding
pointer tags 1 and 2, one object each. The `BuiltinForeign` class is typed through
`Closure`, the other Haskell values stay under `Foreign`, and the C side learns the two
layouts from the probes like the data constructors (`rope_of` and `deque_of` are a tag
compare and a load; bytes keep the two-level form). Nothing on the wire changed, texts and
lists are their own cases of the ANF `Value`, and `Universal.compare` orders the new kinds
as the foreign values they were. The six tagged constructors are `ListC`, `TextC`, `Enum`,
`Data1`, `Data2` and `Data5` (base's `Map` node, native under the JIT: 2 to 5% on the Nat
map rows against the node at tag 7); `Data3` and `Data4` read the info table now, which
the tree rows show. Text and list rows 6 to 8% faster native. `benchmarks/2026-10-15-text-list.md`.
