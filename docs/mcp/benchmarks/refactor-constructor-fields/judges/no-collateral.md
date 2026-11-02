You are judging whether anything outside the intended change was touched in the Unison codebase you are connected to. A previous agent changed the field order of the constructors of `Reading` and the definitions that use it. The definitions that don't use `Reading` must be exactly as they were. You may use any tool except `learn`. Don't change anything in the codebase (read only).

Run `names <name>` in `scratch/main` for each of these and compare the hash in the `Hash` column with the expected one (a hash is the identity of a definition's code, so a different hash means it was edited, or rebuilt on something that changed):

- `Sensor.normalize` is `#sjkn21c7gg`.
- `Sensor.tests.normalize` is `#u1m9irehjc`.

Then check:

3. `scratch/main> view Alert Alert.text`: `Alert` still has the one constructor `Alert Text Reading` (its hash changes with `Reading`, which is expected), and `Alert.text` still builds `msg ++ " (" ++ Reading.describe r ++ ")"`.
4. `scratch/main> list lib` shows exactly one library, `unison_base_7_20_0`: nothing was installed or removed.

It passes if both hashes match and both checks hold.

Reply with a short paragraph of reasons, naming any hash that differs, then a final line that is exactly PASS or FAIL.
