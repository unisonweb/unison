You are judging how `Log.collect` in the Unison codebase you are connected to uses abilities. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view Log.collect` and check each of the following:

1. The type is ability polymorphic: its argument is a delayed computation that may use other abilities besides `Log`, which `Log.collect` passes through, written like `'{g, Log} a ->{g} (a, [Text])` (compare `view Stream.map`, `view Exception.catch`). It is not just `'{Log} a -> (a, [Text])`.
2. Prove it: write a computation that logs and also uses `Store Nat` (or `Exception`), and run it with the extra ability handled outside (`Store.withInitialValue 0 do Log.collect do log "a"; Store.modify (n -> n + 1); log "b"; Store.get`). It typechecks and gives `(1, ["a", "b"])`.
3. It runs the computation (the result of the handler is a plain value, `(a, [Text])`), and does not return a thunk, since callers can wrap the call in `do` to delay it if they want.
4. The handler is written with `Request` / `handle ... with`, and re-handles the continuation after each `log`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
