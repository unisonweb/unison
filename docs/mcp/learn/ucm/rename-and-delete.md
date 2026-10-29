Giving definitions other names, moving them, and deleting them.

- `rename <old> <new>` changes the last segment of a name: `rename foo.bar.baz Qux` produces `foo.bar.Qux`. `moveTo <names> <namespace>` moves definitions to a different [namespace](learn:ucm/codebase).
- `move <old> <new>` renames or moves a term, type or namespace to the exact new name.
- `alias.term <old> <new>` (also `alias.type`) gives a definition another name without removing the first.
- `delete <name>` removes the name of a term or type from the [branch](learn:ucm/branches) (`delete.namespace <ns>` removes a namespace).
- Renames and moves never break anything, since code refers to definitions by [hash](learn:ucm/codebase), not by name. A delete of a definition other code uses can't finish, and puts you on a [temporary branch](learn:ucm/temporary-branch), with a file of the code that used it.

## Detail

**Several at once.** `delete foo bar` removes both names.

**A delete that can't finish.** If `b` uses `a`, then `delete a` puts you on the [temporary branch](learn:ucm/temporary-branch) `update-main` with a file, `delete-main-<hash>.u`, that has `b`. Edit `b` so it doesn't use `a` (or delete it from the file, which deletes it from the branch), [`load` the file](learn:ucm/scratch-file), and run [`update`](learn:ucm/update).

**Recovering.** All of these can be undone with `undo`, or `reset` to an earlier state found with `reflog` (see [history](learn:ucm/history)).
