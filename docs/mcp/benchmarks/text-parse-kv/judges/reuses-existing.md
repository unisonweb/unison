You are judging whether `KV.parse` in the Unison codebase you are connected to makes use of what the base library already offers for text with structure. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view KV.parse`, then `dependencies KV.parse` for what it calls, following the project's own helper definitions). Use `docs Pattern`, `docs Text.patterns` and `find` to see what base offers.

Rate it:

- **GREAT**: the shape of the text is described with base's `Pattern` (`Text.patterns` pieces combined with `join`, `many`, `or`, `capture` and the like, run with `Pattern.run` or captured with a base helper), so the checking and the capturing are one thing.
- **PASS**: it splits with base text functions (`Text.splitOnText` or `Text.split` on `;` and `=`) and checks each piece with base functions such as `Text.all`, with no character-by-character recursion of its own.
- **FAIL**: it walks the characters with a hand-written recursive function or fold, or it accepts text the task says to reject (no checking of the keys).

A solution that does the right thing but ignores a base facility that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
