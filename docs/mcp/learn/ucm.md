UCM, the Unison Codebase Manager, is the command-line tool that stores your code in the [codebase](learn:ucm/codebase) and is how you search, change, test and share it. Here you drive it with commands in [transcripts](learn:ucm/transcript).

- A command is one line in a `ucm` block of a [transcript](learn:ucm/transcript): `> view foo`. A bare `>` reuses the project and branch of the previous command.
- Code lives in [projects, each with branches](learn:ucm/branches). A branch holds the [codebase's](learn:ucm/codebase) definitions, organized into namespaces. Libraries you depend on are under `lib` (see [libraries](learn:ucm/libraries)).
- Code in a `unison` block is typechecked but is not saved until you run [`update`](learn:ucm/update).
- Always give a command its arguments. `view`, `docs`, `edit` and `display` with no arguments open an interactive picker, and nobody can pick.
- `help <command>` shows a command's usage. `help` with no arguments prints help for every command (over a thousand lines, so you get a file path back to grep).

## Detail

**Where a command runs.** `switch myproject/main` changes the project and branch, and so does starting a command line with `myproject/main>`. A project or branch that doesn't exist yet is created, except with `transcript-read-only`, where it must already exist. [`projects` and `branches`](learn:ucm/branches) list what exists.

**Undoing.** Most changes can be undone with `undo`, or by going back to an earlier state found with `reflog` (see [history](learn:ucm/history)).

**Topics.** Start with [transcripts](learn:ucm/transcript) for the format and what the tools allow, and [the codebase](learn:ucm/codebase) for how code is stored.
