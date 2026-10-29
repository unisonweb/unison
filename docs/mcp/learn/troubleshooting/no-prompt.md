The transcript didn't parse: "unexpected newline, expecting '/'" on a line in a `ucm` block means the line has no `>` prompt.

The reply looks like this:

````
The transcript didn't parse:
transcript.md:2:9:
  |
2 | projects
  |         ^
unexpected newline
expecting '/'
````

Every line in a `ucm` block is a [prompt](learn:ucm/transcript) and a command: `myproject/main> projects`, or `> projects` to use the project and branch of the previous line. Add the `>`. Nothing ran, so you can fix the line and send the transcript again.
