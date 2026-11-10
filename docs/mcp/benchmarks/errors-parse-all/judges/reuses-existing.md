You are judging how `parseAll` in the Unison codebase you are connected to reports its failure. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view parseAll` (and `dependencies parseAll` for what it calls, following the project's own helpers), and look at base's `Throw` ability (`find Throw`, `docs Throw`) if you need to.

First check that its type is `[Text] -> Either Text [Nat]` with no abilities, that it doesn't use a function that throws on bad input, and that it reports the first text that isn't a number (not a later one). If any of that fails, the rating is FAIL.

Then rate it:

- **GREAT**: it uses the `Throw` ability: `Throw.throw` (or a helper such as `Optional.orThrow`) with the failing text as the thrown value, converted to an `Either` at the end with `Throw.toEither`. The thrown value is the text itself.
- **PASS**: correct, but done another way: explicit recursion or a fold that builds the `Either`, or `Abort`/`Exception` with a conversion at the end that recovers the text.
- **FAIL**: it fails the checks above.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
