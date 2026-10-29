You are judging how `average` in the Unison codebase you are connected to uses abilities. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view average` and check each of the following:

1. `average` signals the empty list by raising with `Exception` (`Exception.raise`, `Exception.raiseGeneric`, or a base helper such as `ensure`/`bug`), checking for the empty list before dividing.
2. `averageOrNone` is built from `average`, converting its failure to `None` with a base function (`Abort.toOptional`, `Exception.catch` then `Either.toOptional`, or `Exception.toOptional`-style), and does not duplicate the average computation. View both.
3. Neither uses `Abort` or `Exception` in a way that leaks an ability into `averageOrNone`'s type.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
