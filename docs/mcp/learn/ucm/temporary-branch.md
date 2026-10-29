When `update`, `delete`, `merge`, `pull` or `upgrade` can't finish, UCM puts you on a temporary [branch](learn:ucm/branches) with a file of the definitions to fix. This is how to finish, or abandon, the action.

A branch must always compile, so these actions either produce a branch that compiles or don't finish. The actions are [`update`](learn:ucm/update), [`delete`](learn:ucm/rename-and-delete), [`merge`](learn:ucm/merge), [`pull`](learn:ucm/share) and [`upgrade`](learn:ucm/upgrade). When one doesn't finish, UCM switches you to a temporary branch (`update-main`, `merge-bob-into-alice`, `upgrade-old-to-new`; `delete` also uses `update-main`) and writes the definitions that need attention to a file named like `merge-bob-into-alice-368151a8.u`. The reply names both. To finish:

1. Edit the file until it should compile. For a merge, resolve the [conflict markers](learn:ucm/merge/detail).
2. Run `load <file>` on the temporary branch (see [the scratch file](learn:ucm/scratch-file)). If it reports errors, fix them and `load` again, until there are none.
3. Run `update` with no arguments, still on the temporary branch. It saves the definitions in the file into the original branch, merges the temporary branch back, and deletes it.

To abandon the action instead, run `cancel`. It puts you back on the original branch.

## Detail

**The file is a real file** in the directory the server was started in. Its contents are not repeated in the reply, so read and edit it with your file tools. The same action on the same branches always gets the same file name.

**What is in the file** depends on the action:

- `update`: the definitions you updated, and everything that depends on them and no longer typechecks.
- `delete`: everything that depended on what you deleted.
- `upgrade`: the code that no longer typechecks against the new library version.
- `merge` and `pull`: the definitions that both sides changed, with [conflict markers](learn:ucm/merge/detail), then everything that depends on them.

**The definitions in the file are not on the temporary branch.** Everything else is. So [`view` and `find`](learn:ucm/find), which only look at the branch, don't show them. `update` saves what is in the file.

**Deleting a definition from the file** deletes it from the branch when you `update`, because `update` just doesn't add it back. That is how to remove a definition on purpose, and also how to lose one by accident.

**If `update` still can't finish,** the file is rewritten with what still needs fixing and you stay on the temporary branch. Edit, `load`, `update` again.

**An example, in a transcript.** After a merge of `bob` into `alice` leaves you on `merge-bob-into-alice` with the file `merge-bob-into-alice-368151a8.u`, edit the file, then:

````
``` ucm
myproject/merge-bob-into-alice> load merge-bob-into-alice-368151a8.u
```
````

and when that loads with no errors:

````
``` ucm
myproject/merge-bob-into-alice> update
```
````

The server never launches an external merge tool, even if `UCM_MERGETOOL` is set.
