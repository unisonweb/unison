You are judging whether `sumService` in the Unison codebase you are connected to is documented. A previous agent wrote it (a small web service for `POST /sum`). You may use any tool except `learn`. Don't change the project's definitions.

Run `docs sumService`. It must have documentation that says what the service does, in more than its name: the route and method, what the body looks like, what it answers, and what happens for a bad body. The documentation must agree with the code (`view sumService`). You are judging whether it is present and sensible, not how polished it is.

It passes if the documentation exists and covers those points without contradicting the behavior. It fails if there is none, it only repeats the name, or it contradicts the code.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
