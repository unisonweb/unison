You are judging whether `Map.invert` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view Map.invert`, then `dependencies Map.invert` for what it calls, following the project's own helper definitions). Use `find` and `docs` for the `Map` functions that build maps from lists and that combine values.

Rate it:

- **GREAT**: it is a composition of base functions, such as `Map.toList` then `Map.fromListWith` (with the right combining order), or `Map.foldLeft` with `Map.alter`/`Map.insertWith`. No explicit recursion.
- **PASS**: it uses a base fold but with a significantly hand-rolled step, or sorts the lists afterward to repair the order.
- **FAIL**: it hand-rolls the traversal with its own recursion.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
