You are judging the behavior of `Text.slug` and `Page.render` on the branch `alice` of the project `scratch` in the Unison codebase you are connected to. A previous agent brought `Text.slug` over from the branch `bob`. You may use any tool except `learn`. Don't change the project's definitions or branches.

The intended behavior: `Text.slug : Text -> Text` lowercases its argument and replaces every space with `-`. `Page.render : Text -> Text` is `"== Home page ==\n"` followed by the body (with a newline after the heading, as in `"== Home page ==\nhi"`). Run a transcript with the stanza `scratch/alice> ls` followed by a `unison` block with watch expressions, such as `> Text.slug "Hello World"`, whose value is printed, and check:

1. `Text.slug "Hello World"` is `"hello-world"` and `Text.slug "Already-fine"` is `"already-fine"`.
2. `Text.slug "A B  C"` is `"a-b--c"` (two spaces become two dashes) and `Text.slug ""` is `""`.
3. `Page.render "hi"` is `"== Home page ==\nhi"`.

It passes if every check passes. If a definition cannot be found or the code doesn't typecheck, that is a FAIL.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
