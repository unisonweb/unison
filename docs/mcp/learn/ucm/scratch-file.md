The scratch file is the file UCM typechecks your code in before it is saved to the [codebase](learn:ucm/codebase). In a [transcript](learn:ucm/transcript) every `unison` block is checked, and the most recent one is the one [`update`](learn:ucm/update) saves.

- Code you write in a `unison` block is typechecked against the current branch. UCM reports what is new, and any type errors, in a generated block right after yours.
- `update` saves the most recently typechecked block into the branch.
- `load <file>` typechecks a file on disk instead, so that `update` then saves what is in that file, and `update <file>` does both. You need it for the files UCM writes when an action [can't finish](learn:ucm/temporary-branch).
- [`edit <name>`](learn:ucm/find) puts the source of a definition in the scratch file, so that you can change it, and `edit.to <file> <name>` puts it in a file of your own. After a search that numbers its results, `edit 1-3` brings those results in.
- **The files UCM writes are real files**, in the directory the server was started in, or at the path you give (give `edit.to` a path in a directory you are allowed to edit): the file of `edit` and `edit.to`, the file [`rewrite`](learn:ucm/rewrite) rewrites, and the file of an action that [can't finish](learn:ucm/temporary-branch). They stay there for your next calls. `edit.to` adds to the top of a file that exists, leaving out what it already has, so one file can collect the definitions of several commands. The reply shows what was added, not the whole file: read the file itself.
- **A `load` carries over to your next call:** `update` with no arguments saves what the last `load` typechecked, even in a later transcript, and `update work.u` loads the file and updates in one.

## Detail

**In an interactive UCM,** the scratch file is a `.u` file that UCM watches, and it typechecks the file whenever it changes. A transcript has no file to watch, so each `unison` block takes its place, and `load` is how you point UCM at a file on disk.

**Errors** in a `unison` block use line numbers of your transcript. An error in a file you `load` uses that file's own line numbers.

**`load` doesn't save anything.** It typechecks the file and evaluates any tests in it. Run [`update`](learn:ucm/update) to save.
