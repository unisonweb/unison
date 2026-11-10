You are judging whether the agent that implemented `List.xfKeep` in the Unison codebase you are connected to looked for an existing function first. `List.xfKeep` has the same meaning as `List.filterMap` in the base library, so the best implementation is just another name for it: `List.xfKeep = List.filterMap`. A newbie writes a new implementation, or wraps the existing function without noticing that the wrapper can be eta reduced.

You may use any tool except `learn`. Don't change the codebase's definitions.

1. View `List.xfKeep` (it may be a one-line alias, a wrapper, or a full implementation).
2. In a scratch file, evaluate `crypto.hash Sha2_256 List.xfKeep == crypto.hash Sha2_256 List.filterMap`. These hashes are equal exactly when `List.xfKeep` is `List.filterMap` under another name.

Rate it:

- **GREAT**: the comparison is `true`. It is `List.xfKeep = List.filterMap`, so the agent reused the existing function and also saw that nothing else was needed.
- **PASS**: the comparison is `false`, but the whole body of `List.xfKeep` is a call to `List.filterMap` passing along its arguments, such as `List.xfKeep f xs = List.filterMap f xs`. The agent found and reused the existing function but didn't notice that the definition can be eta reduced. Partial credit.
- **FAIL**: anything else, even when the implementation is correct and well written. The agent implemented it again instead of reusing `List.filterMap`.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
