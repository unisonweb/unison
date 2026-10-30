You are judging the end state of the project in the Unison codebase you are connected to. A previous agent was asked, in one sentence, to add a constructor `Shape.RightTriangle Float Float` (the lengths of the two legs) to the type `Shape`. The prompt said no more, but the work is done only when the project compiles with it, every function on shapes gives the right answer for it, the existing tests still pass, and there are tests for the new constructor. You may use any tool except `learn`. Don't change anything in the codebase (no `update`, `switch` or edits); read it with transcripts that only run read commands.

Setup, so you know what to expect: the project `scratch`, branch `main`, had `type Shape = Square Float | Rect Float Float | Circle Float`, the functions `Shape.area` (and `Shape.area.doc`), `Shape.perimeter`, `Shape.scale`, `Shape.hasCurve`, `Shape.corners`, `Shape.isSquare`, `Shape.totalArea`, the helpers `Geometry.pi` and `Geometry.square` (which do not use `Shape`), and the tests `Shape.tests.area`, `Shape.tests.perimeter`, `Shape.tests.scale`, `Shape.tests.corners`. Adding a constructor makes the functions that match on `Shape` without a wildcard stop typechecking, so UCM's `update` puts the worker on a temporary branch (`update-main`) with a file to fix. The work is done only when that has been finished. `Shape.corners` and `Shape.isSquare` match with a wildcard, so they still typecheck after the change, and `Shape.corners` gives the wrong answer for the triangle unless it is edited.

Run these and check each:

1. A bare `> reflog` stanza (no project prefix) lists the changes of the current branch. Its Branch column must say `scratch/main`: the worker must end on `main`.
2. `scratch/main> branches` lists exactly `main`: the temporary branch is gone.
3. `scratch/main> todo` has nothing pending.
4. `scratch/main> view Shape` shows four constructors: `Square Float`, `Rect Float Float`, `Circle Float` and `RightTriangle Float Float` (in any order), with those field types.
5. `scratch/main> find` still shows every definition named above (nothing was deleted to get the update through, the documentation `Shape.area.doc` included), and no other definition apart from new tests (names containing `tests`): no leftover helper and no copy of the old type.
6. `scratch/main> view Shape.area Shape.perimeter Shape.scale Shape.hasCurve Shape.corners`: each has an explicit case for `RightTriangle` (for `Shape.corners` the case may be explicit, or the wildcard may have been replaced by explicit cases for all four constructors; a wildcard that still answers `4` for the triangle fails this check).
7. `scratch/main> test` runs and every test passes; the four original tests are still there (`find tests`), none of them was emptied, and at least one of them or a new test checks a `RightTriangle` result for each of area, perimeter, scale and corners, with the expected value written out (for example `6.0` for the area of legs `3.0` and `4.0`), not computed by calling the function again.

It passes if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
