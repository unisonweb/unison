Giving definitions other names, moving them, and deleting them.

- `move <old> <new>` renames or moves everything called `<old>` to the exact new name: the term, the type and the [namespace](learn:ucm/codebase), whichever of them exist. So `move foo bar` also takes `foo.tests.example` to `bar.tests.example`, and `move Shape Figure` takes the constructors `Shape.Circle` and so on. Use this one to rename, not `move.term` or `move.type`, which leave those behind, and don't rename by changing the name in a file, which makes a new definition and leaves the old one (unless the definition isn't in the branch yet).
- `rename <old> <new>` does the same, but you give only the new last segment: `rename foo.bar.baz Qux` produces `foo.bar.Qux`. `moveTo <names> <namespace>` moves definitions into a different namespace.
- If a term or type already has the new name, `move` and `rename` stop and change nothing. But if `<old>` is a namespace and a namespace already has the new name, the old contents of that one are lost from the branch, with only a warning. Pick a free name, or `undo` the `move`.
- `alias.term <old> <new>` (also `alias.type`) gives a definition another name without removing the first.
- `delete <name>` removes the name of a term or type from the [branch](learn:ucm/branches) (`delete.namespace <ns>` removes a namespace).
- Renames and moves never break anything, since code refers to definitions by [hash](learn:ucm/codebase), not by name. A delete of a definition other code uses can't finish, and puts you on a [temporary branch](learn:ucm/temporary-branch), with a file of the code that used it.

## Detail

**Several at once.** `delete foo bar` removes both names.

**A delete that can't finish.** If `b` uses `a`, then `delete a` puts you on the [temporary branch](learn:ucm/temporary-branch) `update-main` with a file, `delete-main-<hash>.u`, that has `b`. Edit `b` so it doesn't use `a` (or delete it from the file, which deletes it from the branch), [`load` the file](learn:ucm/scratch-file), and run [`update`](learn:ucm/update).

**Recovering.** All of these can be undone with `undo`, or `reset` to an earlier state found with `reflog` (see [history](learn:ucm/history)).
