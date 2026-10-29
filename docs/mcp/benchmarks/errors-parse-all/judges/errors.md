You are judging how `parseAll` in the Unison codebase you are connected to uses abilities. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view parseAll` and check each of the following:

1. It converts with a base function that returns an `Optional` (such as `Nat.fromText`) and turns the `None` into a `Left` with the input text, rather than using a function that throws.
2. It stops at the first failure and reports that text: it does not collect all the failures or report a later one.
3. It doesn't use `Exception` or `Abort` at its top level (its type has no abilities); if it used them internally, they are handled inside it.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
