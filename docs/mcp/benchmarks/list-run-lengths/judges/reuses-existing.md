You are judging whether `List.runLengths` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view List.runLengths`, then `dependencies List.runLengths` for what it calls, following the project's own helper definitions). Use `find` or `docs` to see which grouping functions base has.

Rate it:

- **GREAT**: it is a composition of base functions that do the grouping, such as `List.groupConsecutive` or `List.group` followed by `List.map` for the sizes. No explicit recursion of its own.
- **PASS**: it uses some base functions but also hand-rolls the grouping step, for instance with its own fold.
- **FAIL**: the grouping is a hand-written recursive function or loop that does not use a base grouping function.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
