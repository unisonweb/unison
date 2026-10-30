You are judging how `Tree.label` in the Unison codebase you are connected to keeps its counter. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view Tree.label` (and `dependencies Tree.label` for what it calls, following the project's own helpers) and check:

1. `Tree.label` has a pure type (no abilities left in it).
2. It numbers the leaves in left-to-right order.
3. How the counter is held:
   - **GREAT**: a mutable `Ref` made with `Scope.ref` inside `Scope.run`, created once outside the recursive function and mutated within it (`Ref.read`, `Ref.write` or `Scope.ref.modify`).
   - **PASS**: any other correct way: an ability from base for state, such as `Store`, or the counter passed through the recursion by hand (a helper that takes the next number and returns the new one). The prompt doesn't say how to count, so these are fine, only less direct than a `Ref`.
   - **FAIL**: checks 1 or 2 fail, or the counter is wrong in some other way (for example a list or `Map` rebuilt at each step that gives wrong numbers).

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
