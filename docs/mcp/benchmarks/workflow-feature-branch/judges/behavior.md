You are judging how `Text.shout` behaves in the Unison codebase you are connected to. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions; use scratch code (a `unison` block with watch lines, `> expression`) only for your own checks, and don't switch branches.

Evaluate each of these and note the result:

1. `Text.shout "hey"` is `"HEY!"`.
2. `Text.shout "Hello, World"` is `"HELLO, WORLD!"`.
3. `Text.shout ""` is `"!"` (the exclamation mark is added even to the empty text).
4. `Text.shout "already LOUD"` is `"ALREADY LOUD!"`.
5. `Text.shout "straße"` is whatever the agent's upper-casing gives; don't judge it, but it must not crash.

They pass only if checks 1 to 4 hold.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
