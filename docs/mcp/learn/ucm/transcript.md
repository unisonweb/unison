The format of the transcripts you send, what the four tools allow, and how to read the reply. A transcript is a Markdown document that UCM runs, filling in its output.

- A transcript is Markdown with two kinds of fenced blocks, `ucm` for [UCM](learn:ucm) commands and `unison` for code (see [the scratch file](learn:ucm/scratch-file)):

  ````
  ``` ucm
  myproject/main> find parse
  ```

  ``` unison
  double : Nat -> Nat
  double n = n * 2
  ```
  ````

  Write only the commands and the code. UCM fills in the output, and in a generated `ucm :added-by-ucm` block it reports on each `unison` block (what is new, type errors, the results of tests).
- Every line in a `ucm` block starts with a prompt: `myproject/main> command`, or just `> command` to use the project and branch of the previous line. A line with no `>` doesn't parse. `> projects` and `> branches` list the projects and branches (see [branches](learn:ucm/branches)). A fresh codebase has a project `scratch/main` with no libraries in it yet, so not even `Nat` is defined there until you install the base library (see [libraries](learn:ucm/libraries)).
- Tags go after the language. `:error` says the block is expected to fail, so the transcript carries on. `:hide` runs it without showing its output, and `:hide-all` also hides the block.
- A command that fails in a block without `:error` stops the transcript. The reply is everything up to and including that block, then the error. Fix that spot and run again.
- The tools differ only in what they allow. `transcript-read-only` changes nothing (start here). `transcript` also makes changes that are easy to undo: [`update`](learn:ucm/update), [`merge`](learn:ucm/merge), [branches](learn:ucm/branches), installing [libraries](learn:ucm/libraries), deleting definitions. `transcript-io` also [runs code](learn:ucm/run). `transcript-all` allows everything, including deleting projects and branches and pushing to [Unison Share](learn:ucm/share). A transcript that uses a command its tool doesn't allow doesn't run at all, and the reply says which tool it needs.
- For anything long, write the transcript to a file and pass `transcript-file`, so that you can fix one spot and run it again.

## Detail

**Front matter.** A transcript can start with YAML front matter. `autoupdate: true` runs [`update`](learn:ucm/update) after every `unison` block (it needs `transcript` or a stronger tool).

**Your code isn't repeated.** A `unison` block of more than 10 lines is replaced in the reply by a note like `-- code block 2: your 13 lines, lines 8-20 of your transcript`. Pass `echo: true` to get the blocks back.

**Line numbers.** Errors about your `unison` blocks use line numbers of your transcript (or of the `transcript-file`). An error about a file you [`load`ed](learn:ucm/scratch-file) uses that file's own line numbers.

**Long replies.** A reply of more than 25 lines is written to a temporary file, and you get the path and the line count. Grep it or read a range of it. The error of a failed transcript is always in the reply itself.

**Program output.** What a program run with [`run`](learn:ucm/run) printed to stdout or stderr appears under "Standard output of the transcript" and "Standard error of the transcript".

**Transcripts run in place** on the real [codebase](learn:ucm/codebase), one at a time. What they change stays changed, until you `undo` (see [history](learn:ucm/history)).
