You are judging how a refactoring was done in the Unison codebase you are connected to, and whether it was done. A previous agent was asked to give `bullets` a new first argument, a limit, so that `bullets : Nat -> [Text] -> Text`, and to make every existing use of it pass `maxNat` (no limit) so that nothing changes for them. This change is a good fit for a structural rewrite: one rule, `bullets ==> bulletsLimit maxNat`, rewrites every use, including the bare ones (`List.map bullets`, `|> bullets`, `bullets << f`), in one command (`rewrite.find :rule` to look, `rewrite :rule` to write the result to a file). Doing it by hand, definition by definition through the update's file, is fine but slower and easier to get wrong. You may use any tool except `learn`. Don't change anything in the codebase (read only). A `unison` block of watch expressions is fine for evaluating things.

The worker's submitted transcripts (one Markdown file per call it made to a transcript tool) are in this directory: `{{WORKER_TRANSCRIPTS}}`. Read them with a shell command (for example `ls` and `grep`); this is the only way to see the method, since the codebase does not show it.

Step 1, the end state. In project `scratch`, check:

- `scratch/main> branches` lists only `main` (no temporary branch left).
- `scratch/main> find bullets` shows `bullets : Nat -> [Text] -> Text`.
- `scratch/main> todo` has nothing pending, and `scratch/main> test` passes.
- `scratch/main> view Report.allSections Report.shopping Report.show Report.sideBySide Report.team Report.trimmed bullets.tests.basic`: every use of `bullets` passes `maxNat` (including `List.map (bullets maxNat)`, `|> bullets maxNat`, `bullets maxNat << ...`).

If any of these fails, there is no correct end state: the rating is FAIL (other judges say why in detail).

Step 2, the method, only when step 1 holds. Run a search over the worker's transcripts, for example `grep -rnE '(^|> )(rewrite|rewrite\.find|sfind)( |$)|@rewrite' {{WORKER_TRANSCRIPTS}}`. A hit is a line that runs the command `rewrite`, `rewrite.find` or `sfind` (as a command line in a `ucm` block, with or without the `scratch/main>` prefix) or a rewrite rule written with `@rewrite` in a `unison` block. Mentions of those words in prose do not count. If the directory is missing or empty, say that you could not see the transcripts and rate PASS.

- GREAT: at least one hit, and the end state is correct (the agent used the structural rewrite).
- PASS: no hit, and the end state is correct (the agent did it by hand).
- FAIL: the end state is not correct.

Reply with a short paragraph of reasons (name the transcript file and the line of the first hit, or say there was none), then a final line that is exactly GREAT, PASS or FAIL.
