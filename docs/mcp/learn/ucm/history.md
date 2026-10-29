Looking at what changed in a [branch](learn:ucm/branches), and going back.

- `undo` reverts the most recent change to the branch.
- `reflog` lists every change to the current branch (`project.reflog` and `reflog.global` list those of the project and of everything).
- `reset <#hash>` sets the branch back to an earlier [state](learn:ucm/codebase), found with `reflog`.
- `history [namespace]` shows the history of a [namespace](learn:ucm/codebase).
- `diff.namespace <before> <after>` shows how one namespace differs from another, and `diff.branch <one> <two>` how two branches differ.

## Detail

**Use `reflog` to find a state to `reset` to.** If you make a mistake with `reset`, run `reflog` and `reset` to the earlier state.

**`reset` can also point at a branch:** `reset foo` resets the current namespace to the state of the `foo` namespace.

**Comments in history.** `history.comment <message>` adds a comment to the history.
