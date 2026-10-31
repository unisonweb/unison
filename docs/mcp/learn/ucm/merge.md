Merging one [branch](learn:ucm/branches) into another with `merge`, including how conflicts are resolved.

- To merge branch `bob` into branch `alice`, get on `alice`, then `> merge /bob`. If you aren't on `alice` already, `> switch /alice` first.
- Changes that don't conflict are merged automatically: definitions only one branch added or changed, renames, and so on.
- If both branches changed a definition in different ways, or code that depends on the merged definitions no longer typechecks, the merge can't finish. You are put on a [temporary branch](learn:ucm/temporary-branch), `merge-bob-into-alice`, with a file named like `merge-bob-into-alice-368151a8.u`, and that topic says how to finish.
- The file can be long, because it ends with every definition that depends on a conflicted one, and the reply says how many lines and conflicts it has. Unless it is short, don't read all of it: search it for `-- **` (the notes that start each part of the file) and for `<<<<<<<` (each conflict), and look at only those places. In a conflict, the markers are git's (see the [detail](learn:ucm/merge/detail)): the version in `alice`, then the original, then the version in `bob`. Keep what you want and delete the marker lines.
- The parts, in order: a `-- **` note naming definitions that were merged line by line (if any), the conflicts in name order, a `-- **` note saying that the definitions after it are not conflicted but depend on conflicted ones, and those definitions. When nothing conflicts but dependents no longer typecheck, the file has a `-- **` note saying so and only the dependents.

## Detail

**A conflict looks like this:**

````
foo : Nat
<<<<<<< alice
foo = 2
||||||| base
foo = 1
=======
foo = 3
>>>>>>> bob
````

This is git's `diff3` style. The first part is the version in the branch you are merging into (`alice`), the part after `|||||||` is the common ancestor, and the part after `=======` is the branch being merged in (`bob`).

**The merge is line by line.** The file is a three-way merge of the text of the definitions, so if both branches changed one definition but on different lines, the changes combine with no markers. Look at those definitions too, since the result is a combination neither branch wrote.

**After the markers are gone,** run [`load <file>`](learn:ucm/scratch-file) until it has no errors, then `update`, as in [temporary branches](learn:ucm/temporary-branch). To give up on the merge, run `cancel`.

**Merging from Unison Share.** `pull` is `merge` for a branch on [Unison Share](learn:ucm/share). It leaves you in the same situation when it can't finish.

**Making a branch to merge.** A branch to merge in usually comes from `branch` (see [branches](learn:ucm/branches)): make `bob` from `main`, change it, then `switch /main` and `merge /bob`.
