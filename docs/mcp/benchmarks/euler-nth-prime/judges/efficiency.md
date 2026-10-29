You are judging the efficiency of `nthPrime` in the Unison codebase you are connected to. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Evaluate the following in a scratch file (a `>` watch expression is fine). Each must produce the right answer, and none may take longer than 10 seconds or run out of stack:

1. `nthPrime 10001` is `104743`.
2. `nthPrime 20000` is `224737`.

Also view the implementation and check that where it loops it is tail recursive, or uses a fold or stream, so it doesn't use stack proportional to the input.

It passes if the answers are right and fast, and the implementation is stack safe. Otherwise it fails.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
