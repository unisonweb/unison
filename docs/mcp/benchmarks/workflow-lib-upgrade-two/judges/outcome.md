You are judging the end state of the codebase you are connected to. A previous agent was asked to move the project's code from the 1.0.0 releases of two libraries, `textkit` (folder `textkit_1_0_0` in `lib`) and `tally` (folder `tally_1_0_0`), to their 2.0.0 releases (`textkit_2_0_0`, `tally_2_0_0`), fixing whatever no longer compiled, and to remove the old copies from `lib`. The project is `scratch`, and the branch it should end on is `main`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks. This check needs no network.

Run each of the following, and note the result of each:

1. `list lib`: exactly three folders, `textkit_2_0_0`, `tally_2_0_0` and `unison_base_7_20_0`. Neither `textkit_1_0_0` nor `tally_1_0_0` may be left, and no other library may have been added.
2. `dependencies excite`, `dependencies countThree`, `dependencies report`: every name that comes from `textkit` or `tally` is under `lib.textkit_2_0_0` or `lib.tally_2_0_0` (`excite` and `report` use `shout`, `countThree` uses `empty`, `tick` and `total`); none is under a 1.0.0 folder.
3. `branches`: only `main` is listed. No temporary branch (it would be called `upgrade` or start with `upgrade-`) is left, and `main` is the branch you are on (the prompt of each command shows it, for example `scratch/main>`).
4. `todo`: nothing pending.
5. `test`: the three tests `excite.tests.hi`, `countThree.tests.three` and `report.tests.go` exist in the project and pass. `view excite.tests.hi countThree.tests.three report.tests.go`: their expected values are still `"hi!"`, `3` and `"go! 3"`, and each test calls the project's own function (`excite "hi"`, `countThree`, `report "go"`), not a library function.
6. `ls`: the project still has `excite`, `countThree` and `report` at the top level, each with its test; none was deleted.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
