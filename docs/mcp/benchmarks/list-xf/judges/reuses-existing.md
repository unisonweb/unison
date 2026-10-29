You are judging whether the agent that implemented `List.xf` in the Unison codebase you are connected to looked for an existing function first. `List.xf` has the same meaning as `List.map` in the base library, so the best implementation is just another name for it: `List.xf = List.map`.

You may use any tool except `learn`. Don't change the codebase's definitions.

1. View `List.xf` (it may be a one-line alias or a full implementation).
2. In a scratch file, evaluate `crypto.hash Sha2_256 List.xf == crypto.hash Sha2_256 List.map`. These hashes are equal exactly when `List.xf` is `List.map` under another name.

It passes if that comparison is `true`. It fails if it is `false`, even when the implementation is correct and well written, since the point is whether it reused what already existed.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
