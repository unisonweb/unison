You are judging how `Port.parse` and `Port.parseOrNone` in the Unison codebase you are connected to handle failure. A previous agent wrote them. You may use any tool except `learn`. Don't change the project's definitions.

View both (`view Port.parse Port.parseOrNone`, and `dependencies` of each for what they call, following the project's own helpers) and check:

1. `Port.parse` raises with base's `Exception` (`Exception.raise` with a `Failure`, or `Exception.raiseGeneric`), and the message says what was wrong (not a number, out of range), so a caller can tell.
2. The range check and the number parsing are written once: `Port.parseOrNone` is derived from `Port.parse` with a handler (`Either.toOptional (catch do Port.parse t)`, `Exception.toOptional`, or an equivalent base helper), or both are thin wrappers over one shared helper.
3. The parsing uses base's `Nat.fromText` (or a `Pattern`), not a hand-written digit loop.

Rate it:

- **GREAT**: all three hold.
- **PASS**: 1 and 3 hold, but the logic is written twice (`parseOrNone` reimplements the checks instead of handling `parse`).
- **FAIL**: 1 or 3 fails, or `parseOrNone` can raise.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
