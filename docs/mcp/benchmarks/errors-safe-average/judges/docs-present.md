You are judging whether `average` and `averageOrNone` in the Unison codebase you are connected to are documented. A previous agent wrote them. You may use any tool except `learn`. Don't change the project's definitions.

Run `docs average` and `docs averageOrNone`. Each must have documentation that says what the function does, in more than just its name, and the two should agree with how the functions behave (the empty list, and integer division). You are judging whether the documentation is present and sensible, not how polished it is.

It passes if both functions have such documentation. It fails if either has none, or only repeats its name, or contradicts the behavior.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
