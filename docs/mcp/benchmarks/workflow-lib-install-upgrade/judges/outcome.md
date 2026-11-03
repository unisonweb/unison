You are judging the end state of the codebase you are connected to. The project (`scratch`) had `parseNats`, which uses the JSON library `@unison/json` installed in `lib` as `unison_json_1_2_1`. A previous agent was asked to put the project on the latest release of that library and remove the old copy from `lib`; `parseNats` and its tests had to keep working. The branch it should end on is `main`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

First find out which release is the latest: call the Share tool `share-project-info` with `projectName` `@unison/json` and read `latestRelease` (it was `1.4.2` when this task was written; a later release is also a correct answer, written as a folder name by replacing the dots with underscores, so `1.4.2` is `unison_json_1_4_2`). Call the folder you expect `LATEST`.

Then run each of the following, and note the result of each:

1. `list lib`: exactly two folders, `LATEST` and `unison_base_7_20_0`. `unison_json_1_2_1` must be gone, no older json release such as `unison_json_1_4_1` may be there, and no other library may have been added.
2. `dependencies parseNats`: the names that come from the JSON library are all under `lib.LATEST` (`Decoder.array`, `Decoder.nat`, `Decoder.run`), none is under another json folder, and every name has a readable path (a bare hash such as `#d7jtavn707...` in the list means a dependency lost its name, which fails).
3. `branches`: only `main` is listed, so no temporary branch such as `upgrade-...` is left, and `main` is the branch you are on (the prompt of each command shows it, for example `scratch/main>`).
4. `todo`: nothing pending (no "terms do not have any names", no definitions to fix).
5. `test`: `parseNats.tests.numbers`, `parseNats.tests.empty` and `parseNats.tests.notNumbers` exist and pass, and `view` of each still has its original expected values (`Some [1, 2, 3]`, `Some []`, and `Either.isLeft` for the text `["a"]`).

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
