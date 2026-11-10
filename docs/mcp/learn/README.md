# The `learn` topics

The files in this directory are the source of truth for the MCP server's `learn` tool and for the server instructions. They are embedded in the `unison` executable when it is compiled (`unison-cli/src/Unison/MCP/Learn.hs`), so editing one means rebuilding. This file (`README.md`) is not a topic.

## Layout

- `instructions.md` is the text of the server instructions.
- Every other `.md` file is a topic, and its path is the file's path without `.md`. So `ucm/merge.md` is the topic `ucm/merge`. A topic's subtopics are in the directory with the topic's name: `ucm.md` goes with `ucm/`. Nothing else in the layout is special.
- `index.md` is the root: what `learn("")` returns.
- The subtopics in an index are in alphabetical order, so name files accordingly.

## A topic file

- It starts with a blurb: one or two sentences, in a paragraph of their own. The blurb is what the index of the parent topic shows.
- Then the summary: the minimum you need to know.
- Then, optionally, a `## Detail` heading, and the details. `learn("ucm/merge")` returns the blurb and summary, then an index of the subtopics. `learn("ucm/merge/detail")` returns the Detail section.
- `Detail` is the only heading that means something. Inside a Detail section use bold labels, not headings.
- HTML comments (`<!-- ... -->` on lines of their own) are dropped.

## Links, and what to write

A link to a topic is a Markdown link whose URL is `learn:` followed by the topic's path, like `[merging](learn:ucm/merge)`, or `learn:ucm/merge/detail` for a Detail section. The tests check that every link goes to a topic.

The reader is a general-purpose LLM that has never seen Unison. A term it wouldn't know (UCM, codebase, branch, namespace, transcript, scratch file, a command, and so on) is linked to the topic that explains it the first time it is used in the summary of a topic, and again the first time it is used in that topic's Detail section, since the two are fetched separately. A topic doesn't need to link to itself.
