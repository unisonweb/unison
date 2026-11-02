You are judging how a refactoring was done in the Unison codebase you are connected to, and whether it was done. A previous agent was asked to put the value before the sensor name in both constructors of `type Reading = Temp Text Float | Humidity Text Float`, and to change everything that builds or matches on a reading accordingly. This change is a good fit for a structural rewrite: `signature`, `case` and `term` rules for the two constructors (`case Reading.Temp name v ==> Temp2 v name`, `term Reading.Temp name v ==> Temp2 v name`, plus rules for the bare and partially applied constructors) rewrite every pattern and construction in one `rewrite` command, including the ones in the doc and the tests. Doing it by hand, definition by definition through the update's file, is fine but slower and easier to get wrong. You may use any tool except `learn`. Don't change anything in the codebase (read only). A `unison` block of watch expressions is fine for evaluating things.

The worker's submitted transcripts (one Markdown file per call it made to a transcript tool) are in this directory: `{{WORKER_TRANSCRIPTS}}`. Read them with a shell command (for example `ls` and `grep`); this is the only way to see the method, since the codebase does not show it.

Step 1, the end state. In project `scratch`, check:

- `scratch/main> branches` lists only `main`.
- `scratch/main> view Reading` shows `Temp Float Text` and `Humidity Float Text`.
- `scratch/main> todo` has nothing pending, and `scratch/main> test` passes.
- A `unison` block with the watch expression `> Reading.describe (Temp 21.5 "t1")` gives `"t1: 21.5 C"`.

If any of these fails, there is no correct end state: the rating is FAIL (other judges say why in detail).

Step 2, the method, only when step 1 holds. Run a search over the worker's transcripts, for example `grep -rnE '(^|> )(rewrite|rewrite\.find|sfind)( |$)|@rewrite' {{WORKER_TRANSCRIPTS}}`. A hit is a line that runs the command `rewrite`, `rewrite.find` or `sfind` (as a command line in a `ucm` block, with or without the `scratch/main>` prefix) or a rewrite rule written with `@rewrite` in a `unison` block. Mentions of those words in prose do not count. If the directory is missing or empty, say that you could not see the transcripts and rate PASS.

- GREAT: at least one hit, and the end state is correct (the agent used the structural rewrite).
- PASS: no hit, and the end state is correct (the agent did it by hand).
- FAIL: the end state is not correct.

Reply with a short paragraph of reasons (name the transcript file and the line of the first hit, or say there was none), then a final line that is exactly GREAT, PASS or FAIL.
