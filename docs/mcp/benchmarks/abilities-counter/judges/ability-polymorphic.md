You are judging the type of `Counter.from` in the Unison codebase you are connected to. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Check:

1. `view Counter.from`: its type is `Nat -> '{g, Counter} r ->{g} r` or equivalent: the computation's other abilities, named by a variable, pass through to the result. A type that pins the computation to `'{Counter} r` alone fails.
2. Run a computation under `Counter.from` that uses another ability, handled outside it. For example, with base's `Store`: `Store.withInitialValue "" do Counter.from 0 do (Store.put (Nat.toText (Counter.next())); Store.get())` (adapt to the actual handler names; find them with `find.all Store.`). It must typecheck and give `"0"` or equivalent, which it only does when `Counter.from` lets `Store` through.
3. `view Counter.from` shows a handler written with `handle ... with` on a `Request {Counter} r`, or an equivalent base helper, that carries the count along and resumes.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
