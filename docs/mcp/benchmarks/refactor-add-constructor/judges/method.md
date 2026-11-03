You are judging how a change was made in the Unison codebase you are connected to, and whether it was made. A previous agent was asked to add a constructor, `Shape.RightTriangle Float Float`, to `type Shape = Square Float | Rect Float Float | Circle Float`, which means a new case in every function that matches on a shape, including the two whose match ends in a wildcard (`Shape.corners` answers 4 for anything that isn't a circle, which is wrong for a triangle). The efficient way to find those functions is a structural search: a `case` rule naming an existing constructor, `matches = @rewrite case Shape.Circle ==> Shape.Circle`, and `rewrite.find :matches`, which lists every definition with a pattern on `Circle`, wildcards in the arguments included; or `dependents Shape`. Changing the type and letting `update` list what no longer typechecks also works, but misses the wildcard functions. You may use any tool except `learn`. Don't change anything in the codebase (read only). A `unison` block of watch expressions is fine for evaluating things.

The worker's submitted transcripts (one Markdown file per call it made to a transcript tool) are in this directory: `{{WORKER_TRANSCRIPTS}}`. Read them with a shell command (for example `ls` and `grep`); this is the only way to see the method, since the codebase does not show it.

Step 1, the end state. In project `scratch`, check:

- `scratch/main> branches` lists only `main`, and `scratch/main> todo` has nothing pending.
- `scratch/main> view Shape` has four constructors, `RightTriangle Float Float` among them.
- A `unison` block with the watch expressions `> Shape.corners (RightTriangle 3.0 4.0)` and `> Shape.area (RightTriangle 3.0 4.0)` gives `3` and `6.0`.
- `scratch/main> test` passes.

If any of these fails, there is no correct end state: the rating is FAIL (other judges say why in detail).

Step 2, the method, only when step 1 holds. Run a search over the worker's transcripts, for example `grep -rnE '(^|> )(rewrite\.find|sfind)( |$)|@rewrite +case|(^|> )dependents +Shape|(^|> )(find|find\.all|find-in)( |$).*Shape' {{WORKER_TRANSCRIPTS}}`. A hit is a `rewrite.find` or `sfind` command with a `case` rule, a `dependents Shape` command, or a `find`, `find.all` or `find-in` that lists the `Shape` namespace (the functions are in it), as a command line in a `ucm` block (with or without the `scratch/main>` prefix), in a transcript that comes before the one that changes the type (the files are numbered or dated; use `ls -tr`). Mentions in prose do not count. If the directory is missing or empty, say that you could not see the transcripts and rate PASS.

- GREAT: at least one hit before the change, and the end state is correct (the agent found the functions to change before changing the type).
- PASS: no hit, and the end state is correct (the agent worked from the update's file, or read the functions one by one).
- FAIL: the end state is not correct.

Note: the `learn` topics teach the method rated GREAT, so GREAT records that the worker followed them, not that it found the method unaided.

Reply with a short paragraph of reasons (name the transcript file and the line of the first hit, or say there was none), then a final line that is exactly GREAT, PASS or FAIL.
