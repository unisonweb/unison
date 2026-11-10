You are judging whether anything outside the intended change was touched in the Unison codebase you are connected to. A previous agent added a constructor to the type `Shape` and extended the functions on it. `Geometry.pi` and `Geometry.square` do not use `Shape` and must be exactly as they were. You may use any tool except `learn`. Don't change anything in the codebase (read only).

Run `names <name>` in `scratch/main` and compare the hash in the `Hash` column with the expected one (a different hash means the definition was edited):

- `Geometry.pi` is `#fj9bgcuefk` (the same value as base's `Float.pi`, so the reply may list that name too).
- `Geometry.square` is `#edjnavi27l`.

Then check:

3. `scratch/main> view Shape.isSquare` is still a match with a `Square` case that returns `true` and a wildcard that returns `false` (a correct function that did not need to change; it may have been rewritten with an explicit case for each constructor, which is also fine, as long as it is right).
4. `scratch/main> list lib` shows exactly one library, `unison_base_7_20_0`: nothing was installed or removed.
5. `scratch/main> branches` lists only `main`, and `> projects` lists only `scratch`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
