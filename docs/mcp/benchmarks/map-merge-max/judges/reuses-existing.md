You are judging whether `Map.mergeMax` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view Map.mergeMax`, then `dependencies Map.mergeMax` for what it calls, following the project's own helper definitions). Use `docs Map` and `find` to see how base combines two maps.

Rate it:

- **GREAT**: it is a base function that combines two maps with a function for the keys in both (`Map.unionWith` or the like) given `Nat.max` or an equivalent lambda, and nothing else.
- **PASS**: it builds the result with other base `Map` functions (a fold over one map inserting into the other with `Map.alter` or `Map.insertWith`, say), with no recursion of its own.
- **FAIL**: it converts the maps to lists and walks them with a hand-written recursive function, or otherwise reimplements the merge.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
