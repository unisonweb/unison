Tests a merge whose file is too long to read comfortably: the same conflict as `workflow-merge-conflict`, with 60 dependents of `Order.total` (`Report.line01` to `Report.line60`) that the merge writes after the conflicts. The question is how an agent works with a long generated file, which the short merge tasks can't show: their files are under 25 lines, so printing them is the right call.

- Good run: `merge /bob`, find the conflicts with a search for `<<<<<<<` and `-- **` (`ucm/merge` says to), read only those parts, edit them in place, leave the dependents alone, `load`, `update`, finish on `alice`.
- Traps: reading the whole file; retyping it; dropping the dependents, because an `update` of a file without them deletes them (`validation/bad-dropped-dependents.md`, which `outcome` fails).
- Judges: `outcome`, `behavior`, `approach`, the same as `workflow-merge-conflict` with the dependents added. No judge sees how the file was read: that is in the worker's log, so `harness/fileread.py <round> <task>` reports it. The judges only check that nothing was lost.
- `setup.md` and `validation/*.md` are long because the 60 dependents are written out.
- Offline.
- Validation: `good.md` gives the judges' values; `bad-dropped-dependents.md` deletes all 60 dependents and still ends clean on `alice`, with its 2 tests passing, so only `outcome` catches it.
- The merge reply gives the file's line and conflict counts. Before it did, 6 of 10 runs searched the file; after, 8 of 8 (see `results.md`).
