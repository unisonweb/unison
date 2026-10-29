You are judging whether the agent that implemented `List.sift` in the Unison codebase you are connected to looked for an existing function first. `List.sift` has the same meaning as `List.filter` in the base library, so the best implementation is just another name for it: `List.sift = List.filter`.

You may use any tool except `learn`. Don't change the codebase's definitions.

1. View `List.sift` (it may be a one-line alias or a full implementation).
2. In a scratch file, evaluate `crypto.hash Sha2_256 List.sift == crypto.hash Sha2_256 List.filter`. These hashes are equal exactly when `List.sift` is `List.filter` under another name.

It passes if that comparison is `true`. It fails if it is `false`, even when the implementation is correct and well written, since the point is whether it reused what already existed.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
