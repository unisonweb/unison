Saving code from a `unison` block into the [branch](learn:ucm/branches) with `update`, and what happens when it can't finish.

- A `unison` block is only typechecked (see [the scratch file](learn:ucm/scratch-file)). `update` adds everything in the most recently typechecked block to the branch, replacing definitions of the same name and updating what depends on them. `add` is another name for `update`.
- With no arguments, `update` uses the most recent block. Run it in a `ucm` block after the `unison` block that defines the code, in the same transcript: a later transcript doesn't remember the block, and `update` there says there is nothing to add.
- **Change what you are changing, and no more.** Send the new definition, run `update`, then `test`. `update` carries a change through everything that depends on it, so don't read all the dependents first or repeat them in your block. What needs your attention will show up: a dependent that no longer typechecks stops the `update`, and a test whose expected value changed fails in `test`.
- If a definition that depends on your change no longer typechecks, `update` can't finish. It puts you on a [temporary branch](learn:ucm/temporary-branch) with a file of the definitions to fix.
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

**`but I expected: #qns7gmlrq1`.** A type error with a hash in it means your block changed a type and also uses a definition from the branch that still has the old type. The hash is the old type. Leave that use out of the block: change the type, `update`, and fix the dependents in the file you get.

**Dependents that aren't in the file.** If you change a definition in the file that has a dependent outside the file, `update` can't finish again. It rewrites the file with the dependents that still need fixing, and you stay on the temporary branch.

**Tests in the block.** A `test>` line in a `unison` block defines a test that runs when the block is typechecked (see [testing](learn:testing)). `update` saves it like any other definition.
