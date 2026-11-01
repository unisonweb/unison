Looking at what changed in a [branch](learn:ucm/branches), and going back.

- `undo` reverts the last command that changed the [branch](learn:ucm/branches), and `redo` restores what `undo` reverted. Repeat them to step further, in any mix. A new change clears what could be redone. They go back no further than the last `reset`.
- `reflog` lists every change to the current branch (`project.reflog` and `reflog.global` list those of the project and of everything).
- `reset <#hash>` sets the branch back to an earlier [state](learn:ucm/codebase), found with `reflog`.
- `history [namespace]` shows the history of a [namespace](learn:ucm/codebase).
- **To look at an earlier version without leaving the branch, or to take something back from it,** copy that state into a namespace of the current branch: `fork #<hash> oldversion` (the hash comes from `reflog` or `history`), then `ls oldversion` and `view oldversion.foo`. Move what you want out of it, `move oldversion.foo foo`, and `delete.namespace oldversion` when you are done. See below.
- `diff.namespace <before> <after>` shows how one namespace differs from another, and `diff.branch <one> <two>` how two branches differ.

## Detail

**How far `undo` goes.** It steps back through the branch's `reflog`, so it works across transcripts and sessions, and one `undo` reverses a whole command, a `merge` or a `move` included. A `reset` is a line it won't cross: right after one, `undo` and `redo` both say there is nothing to do, and `undo` can't bring back the state you reset from. For that, use `reflog` and `reset`.

**Use `reflog` to find a state to `reset` to.** If you make a mistake with `reset`, run `reflog` and `reset` to the earlier state.

**`reset` can also point at a branch:** `reset foo` resets the current namespace to the state of the `foo` namespace.

**Comments in history.** `history.comment <message>` adds a comment to the history.

**Exploring an old version, and cherry-picking from it.** `reset` moves the whole branch, which loses later work. To keep the branch as it is and bring back only some definitions:

````
``` ucm
> reflog
> fork #j5ae1dgiq1 oldversion
> ls oldversion
> view oldversion.Cfg.parse oldversion.Cfg.parse.tests.example
```
````

`fork #<hash> <name>` puts the state at that hash in a namespace `oldversion` of the current branch (the hash is written with a `#`; take it from `reflog` or `history`). Nothing else changes, and `view` and `docs` work on names under it. To take a definition back:

- If its current name is free (or you delete the current one first, which works only if nothing depends on it), `move oldversion.Cfg.parse.tests.example Cfg.parse.tests.example`.
- If the name is taken and others depend on it, retype the old definition in a `unison` block (copy it from `view`) and [`update`](learn:ucm/update), so the dependents follow.

Finish with `delete.namespace oldversion`: it holds a full copy of the old state, `lib` included, and you don't want it left in the branch. `history oldversion` also works, to see what happened in it.

**Cherry-picking from another branch** works the same way, without a hash: `fork /bob: bobsversion` (note the colon) copies the state of the branch `bob` into a namespace of this one (see [branches](learn:ucm/branches)), and you `move` the definitions you want out of it, then `delete.namespace bobsversion`. For a function and its test that only exist in `bob`, that is simpler and safer than a [merge](learn:ucm/merge).
