You are judging whether `Text.numbers` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view Text.numbers`, then `dependencies Text.numbers` for what it calls, following the project's own helper definitions). Use `find` and `docs` for `Text.patterns` and `Pattern` to see what is there.

Rate it:

- **GREAT**: it is built on `Pattern`: a pattern from `Text.patterns` (such as `Text.patterns.int` or `digit`) with `Pattern.captures`, `Text.capturePatternsWith`, or `Text.splitOnPattern`.
- **PASS**: it is built from other base `Text` functions (splitting, filtering characters, `Int.fromText`) without writing a character-by-character loop.
- **FAIL**: it scans the text character by character with its own recursion.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
