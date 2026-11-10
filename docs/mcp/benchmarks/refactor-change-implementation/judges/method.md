You are judging how a change was made in the Unison codebase you are connected to, and whether it was made. A previous agent was asked to change the implementation of `Text.slug : Text -> Text` (collapse runs of dashes, drop leading and trailing ones) without changing its type, so that everything using it goes on working. In Unison that kind of change needs nothing but the new `slug`: `update` carries it through to every definition that uses it. The efficient way is to send the new definition of `Text.slug` (and the new tests), `update`, and `test`; reading or retyping its users (`Post.url`, `Post.urls`, `Post.anchor`, `Site.index`) is wasted work, and retyping them risks changing them. You may use any tool except `learn`. Don't change anything in the codebase (read only).

The worker's submitted transcripts (one Markdown file per call it made to a transcript tool) are in this directory: `{{WORKER_TRANSCRIPTS}}`. Read them with a shell command (for example `ls` and `grep`); this is the only way to see the method, since the codebase does not show it.

Step 1, the end state. In project `scratch`, check:

- `scratch/main> branches` lists only `main`, and `scratch/main> todo` has nothing pending.
- A `unison` block with the watch expressions `> Text.slug "  Hello --  World! "` and `> Text.slug "a - b"` gives `"hello-world"` and `"a-b"`.
- `scratch/main> test` passes.

If any of these fails, there is no correct end state: the rating is FAIL (other judges say why in detail).

Step 2, the method, only when step 1 holds. Search the worker's transcripts for definitions of the users of `slug` that it sent back to UCM: `grep -rnE '^(Post\.(url|urls|anchor)|Site\.index) *[:=]' {{WORKER_TRANSCRIPTS}}` (a definition is a line starting with the name followed by `:` or `=`, inside a `unison` block), and for commands that pull them in: `grep -rnE '(^|> )edit\.dependents|(^|> )edit(\.to)? .*(Post|Site)' {{WORKER_TRANSCRIPTS}}`. Mentions in prose or in `view`/`dependents` commands do not count: reading the users is fine, sending them back is what this measures. If the directory is missing or empty, say that you could not see the transcripts and rate PASS.

- GREAT: no hit. The worker changed `Text.slug` (and tests) and let `update` do the rest.
- PASS: hits, but the end state is correct: the worker retyped or re-sent the users of `slug`.
- FAIL: the end state is not correct.

Note: the `learn` topics teach the method rated GREAT, so GREAT records that the worker followed them, not that it found the method unaided.

Reply with a short paragraph of reasons (name the transcript file and the line of the first hit, or say there was none), then a final line that is exactly GREAT, PASS or FAIL.
