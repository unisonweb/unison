You are judging how `Coro.feed` in the Unison codebase you are connected to uses abilities. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view Coro.feed` and check each of the following:

1. The type is ability polymorphic: both thunk arguments and the returned thunk share an ability variable that lets other abilities through, written like `'{g, Stream b} r -> '{g, Coro a b} r2 -> '{g, Stream a} r3` (compare with `view Stream.map`). It is not `'{Stream b}`/`'{Coro a b}` with no `g`, which would reject computations that also use, say, `Exception` or `Store`.
2. Prove it: in a scratch file write a coroutine that also uses `Exception` or `Store Nat` (for example it counts the yields with `Store.modify`), a producer, and run `Coro.feed` on them with the extra ability handled outside (`Store.withInitialValue 0 do Stream.toList (Coro.feed producer coroutine)`). It must typecheck and give the right output. A signature that doesn't allow this fails.
3. The function returns a delayed computation (a thunk) because it returns a `Stream` and is meant to be composed lazily, whereas a handler that produces a plain value does not return a thunk.
4. The handler is written with `Request`/`handle ... with` and recurses after resuming the continuation; it doesn't read the whole producer into a list first (view it; using `Stream.toList` or `Stream.uncons` on the producer to pull elements one at a time is fine, but materialising the entire producer before feeding anything is not).

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
