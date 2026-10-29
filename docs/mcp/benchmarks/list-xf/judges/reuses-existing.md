You are judging whether the agent that implemented `List.xf` in the Unison codebase you are connected to looked for an existing function first. `List.xf` has the same meaning as `List.map` in the base library, so the best implementation is just another name for it: `List.xf = List.map`. A newbie writes a new implementation, or wraps the existing function without noticing that the wrapper can be eta reduced.

You may use any tool except `learn`. Don't change the codebase's definitions.

1. View `List.xf` (it may be a one-line alias, a wrapper, or a full implementation).
2. In a scratch file, evaluate `crypto.hash Sha2_256 List.xf == crypto.hash Sha2_256 List.map`. These hashes are equal exactly when `List.xf` is `List.map` under another name.

Rate it:

- **GREAT**: the comparison is `true`. It is `List.xf = List.map`, so the agent reused the existing function and also saw that nothing else was needed.
- **PASS**: the comparison is `false`, but the whole body of `List.xf` is a call to `List.map` passing along its arguments, such as `List.xf f xs = List.map f xs`. The agent found and reused the existing function but didn't notice that the definition can be eta reduced. Partial credit.
- **FAIL**: anything else, even when the implementation is correct and well written. The agent implemented it again instead of reusing `List.map`.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
