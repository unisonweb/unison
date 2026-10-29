This server runs Unison [transcripts](learn:ucm/transcript). These are Markdown documents with two kinds of fenced code blocks: `ucm` blocks give commands for [UCM](learn:ucm) (the Unison Codebase Manager), one per line, as in `myproject/main> view foo`, and `unison` blocks give code to typecheck and expressions to evaluate. Anything you can do at the UCM command line you can do here. You get the transcript back with UCM's output after each command.

The Unison `learn` MCP tool explains all of this in whatever level of detail you want. Call it with a topic path, like `learn("ucm/merge")`, or with an empty path for an overview of all the topics. A topic gives a short summary and an index of its subtopics. Add `/detail` to the path for more, as in `ucm/merge/detail`. A link like [transcripts](learn:ucm/transcript) points to a topic: the text after `learn:` is the path to pass to `learn`. Look up topics as you need them, starting with [transcripts](learn:ucm/transcript) before your first call, and whenever something is unfamiliar.

The essentials:

- The four `transcript` tools differ only in which commands they allow within `ucm` blocks. Start with `transcript-read-only`; if a command isn't allowed, nothing runs and the reply names the tool you need. See [the tools](learn:ucm/transcript).
- Code in a `unison` block is only typechecked. It isn't saved or visible to subsequent blocks until you run [`update`](learn:ucm/update).
- Always give a command its arguments (`view foo`, never bare `view`), and don't write the output yourself.
- For longer inputs, write the transcript to a temp file and pass `transcript-file`, so if there are errors you can fix one spot and easily run it again.
- In a `ucm` block, `help <command>` shows a command's usage. `help` with no arguments prints help for every command (over a thousand lines, so you get a file path back to grep). Mark a block ```` ``` ucm :error ```` if you expect it to fail.
