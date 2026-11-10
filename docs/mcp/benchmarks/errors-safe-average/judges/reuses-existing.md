You are judging how `average` and `averageOrNone` in the Unison codebase you are connected to are built and how they fail. A previous agent wrote them. You may use any tool except `learn`. Don't change the project's definitions.

View both with `view average` and `view averageOrNone` (and `dependencies` for what they call).

First check the basics. If any fails, the rating is FAIL:
- `average []` fails properly (not a crash, hang or number) and `averageOrNone []` is `None`.
- `averageOrNone` is built from `average`, converting its failure to `None`. It doesn't recompute the sum and length itself.
- `averageOrNone`'s type has no abilities.

Then rate:

- **GREAT**: `average` signals the empty list with the `Exception` ability (`Exception.raise`, `Exception.raiseGeneric`, or a helper that raises), and `averageOrNone` converts that failure with a base function (`Exception.catch` then `Either.toOptional`, or similar).
- **PASS**: correct, but `average` uses another ability in its type for the empty list, such as `Abort` or `Throw`. (`Exception` is the preferred ability for a function that fails in a user-facing signature; `Abort` is mostly for control flow.)
- **FAIL**: it fails the basics above.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
