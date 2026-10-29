You are judging whether the agent that implemented `List.xfKeep` in the Unison codebase you are connected to looked for an existing function first. `List.xfKeep` has the same meaning as `List.filterMap` in the base library, so the best implementation is just another name for it: `List.xfKeep = List.filterMap`.

You may use any tool except `learn`. Don't change the codebase's definitions.

1. View `List.xfKeep` (it may be a one-line alias or a full implementation).
2. In a scratch file, evaluate `crypto.hash Sha2_256 List.xfKeep == crypto.hash Sha2_256 List.filterMap`. These hashes are equal exactly when `List.xfKeep` is `List.filterMap` under another name.

It passes if that comparison is `true`. It fails if it is `false`, even when the implementation is correct and well written, since the point is whether it reused what already existed.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
