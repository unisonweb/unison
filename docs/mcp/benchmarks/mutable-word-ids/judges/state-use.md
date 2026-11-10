You are judging how `wordIds` in the Unison codebase you are connected to keeps track of the words it has seen. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view wordIds` (and `dependencies wordIds` for what it calls, following the project's own helpers) and check:

1. `wordIds` has a pure type, `[Text] -> [Nat]`, with no abilities left in it.
2. The numbers follow first appearance, and the output has one number per input word.
3. How the seen words are held:
   - **GREAT**: the state lives in one place that the walk updates as it goes, and the walk is a base traversal, not a recursion of its own: a `Map` from word to number held in a mutable `Ref` inside `Scope.run` and read and written by a `List.map` over the words, or a single `List.foldLeft` (or `mapAccumulate`-style base function) whose accumulator is the `Map` and the numbers so far.
   - **PASS**: any other correct way, such as a hand-written recursive helper that passes the `Map` in and out, or a base `Store` ability.
   - **FAIL**: checks 1 or 2 fail, or the lookup rescans the whole input for every word (quadratic, such as finding the index of the word's first occurrence in the list each time).

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
