Saving code from a `unison` block into the [branch](learn:ucm/branches) with `update`, and what happens when it can't finish.

- A `unison` block is only typechecked (see [the scratch file](learn:ucm/scratch-file)). `update` adds everything in the most recently typechecked block to the branch, replacing definitions of the same name and updating what depends on them. `add` is another name for `update`.
- With no arguments, `update` uses the most recent block, or the file of the last `load`. Run it in a `ucm` block after the `unison` block that defines the code, in the same transcript: a later transcript doesn't remember the block, and `update` there says there is nothing to add. A `load` does carry over, and `update work.u` loads the file and updates in one.
- **Change what you are changing, and no more.** Send the new definition, run `update`, then `test`. `update` carries a change through everything that depends on it, so don't read all the dependents first or repeat them in your block. What needs your attention will show up: a dependent that no longer typechecks stops the `update`, and a test whose expected value changed fails in `test`.
- If a definition that depends on your change no longer typechecks, `update` can't finish. It puts you on a [temporary branch](learn:ucm/temporary-branch) with a file of the definitions to fix. When the fix is the same in every one of them (a new argument, a changed constructor), [`rewrite`](learn:ucm/rewrite) can make it; and a change like that is often easier made before the `update`, the way [refactoring](learn:skills/refactor) describes.
- `diff.update` shows what `update` would change, without changing anything.

## Detail

**An update that can't finish.** Suppose `incrFoo` uses `Foo`, and you change `Foo`:

````
``` unison
unique type Foo = Bar Nat Nat
```

``` ucm
> update
```
````

`incrFoo` no longer typechecks, so the reply says you are on a [temporary branch](learn:ucm/temporary-branch), `update-main`, and names a file, like `update-main-90f1b725.u`, that holds the new `Foo` and the `incrFoo` to fix. Fix `incrFoo` in the file, [`load`](learn:ucm/scratch-file) the file until it has no errors, and run `update` again on the temporary branch. That saves everything in the file, merges back into `main`, and deletes the temporary branch.

**`Added to the file, from the codebase: ...`.** When a file changes a type and also uses a definition of the branch that uses the old type, `load` adds that definition to the file (above the fold, under a comment saying why), so that there is one `Shape` in play; it is typechecked with the file, and `update` saves it rebuilt on the new type. For a `unison` block, which is no file, the reply says `Also typechecking, from the codebase` instead, and the definition is typechecked with the block without being shown. A type error where a name has a hash on it, `Shape#qns7gmlrq1`, means something still mentions the branch's old `Shape` next to yours: leave that use out of the block, or `update` first and fix the dependents in the file you get.

**Dependents that aren't in the file.** If you change a definition in the file that has a dependent outside the file, `update` can't finish again. It rewrites the file with the dependents that still need fixing, and you stay on the temporary branch.

**Tests in the block.** A `test>` line in a `unison` block defines a test that runs when the block is typechecked (see [testing](learn:testing)). `update` saves it like any other definition.
