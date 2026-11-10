# 2026-10-15: record field positions as two sorted arrays

A record shape's field positions were an `EnumMap FieldRef Int` (an `IntMap` of boxed
`Int`s), walked by the interpreter's `RecUnpack` and `RecSet` and, node by node, by
`unison_jit_rec_pos`. They are now two unboxed arrays, the field numbers ascending and the
slot of each, searched by binary search on both sides (`shapeSlot` in `MCode.hs`, the helper
in `jit_helpers.c`; "Records" in design.md). Optimized build, a builtins-only transcript
(`scratchpad/recbench3/rec.md`: the suite's two record rows with their own timer), seven
samples of ten calls each; the table shows the median of the last five. The wide row is new
with this change, so it has no earlier figure.

| | interpreter | JIT, records as exits (`UNISON_JIT_DISABLE=records`) | JIT |
| --- | --- | --- | --- |
| Records: a literal, an update, a pattern and two getters, 100000 times | 94.4 ms | 94.7 ms | 7.5 ms |
| Records: a 16-field literal and four getters, 100000 times | 73.4 ms | 73.1 ms | 5.24 ms |

The first row was 7.9 ms native with the map ([2026-10-15-records.md](2026-10-15-records.md)):
79 ns per iteration, now 75, over seven field lookups per iteration in records of one to
three fields, where the map had two or three nodes. That is the small end of the table in
the plan, where the arrays were expected to save a nanosecond or two per lookup, and the
rest of the iteration (three allocations, the update's copy, the arithmetic) is the same.
The wide row is where the map was deepest (16 keys, a walk of four or five nodes per
lookup): 52 ns per iteration native for a 16-field literal (its 16 `Val` boxes, the array
and the record in one chunk) and four lookups of four compares each.

The interpreter's first row reads 94.4 ms against 91.1 ms on the earlier run; the two runs
are from different sessions of the machine and the earlier binary is gone, so whether the
3% is the binary search against the map walk in the interpreter or run-to-run variation was
not established. Records as exits cost what the interpreter does, as before.

The record probe (`unison_jit_record_init`) no longer depends on the handler-environment
probe's `IntMap` constructors, and the boxed `Int` fact is gone from `RK`.
