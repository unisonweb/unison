You are judging the documentation of `List.pick` in the Unison codebase you are connected to. A previous agent wrote `List.pick` and its documentation. Your job is to decide whether that documentation is enough for someone to use the function.

Rules:

- You may call the `learn` tool only for these topics: `ucm/scratch-file`, `ucm/transcript`, `lang`, `lang/terms`, `lang/functions`, `lang/collections/list`. Do not read any other topic, and in particular not `lang/documentation` or `testing`.
- Do not look at the source of `List.pick` or its tests. Only the `docs` output counts.

Steps:

1. Run `docs List.pick` and read the output.
2. Using only that output, write a short Unison program that calls `List.pick` on an input of your own choosing (not one copied from the documentation), and run it so that it produces output.
3. Compare the result with what you expected from reading the documentation. Note anything that was unclear, missing, wrong, or that you had to guess.

The documentation passes if the call worked the first time, the output was what the documentation led you to expect, and you didn't have to guess about the arguments, the order of the arguments, or what the result means. It fails otherwise, including when the documentation is empty, only repeats the name, or contains an example whose result is wrong.

Reply with a short paragraph of reasons, then a final line that is exactly PASS or FAIL.
