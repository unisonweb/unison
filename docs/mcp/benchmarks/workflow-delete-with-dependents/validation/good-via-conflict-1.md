Part 1 of 2. The worker deletes `Page.size` first: UCM can't finish the delete and leaves the codebase on `update-main` with a file (`delete-main-<hash>.u`) of everything that depends on it, all four layers, not only the two that use `Page.size` directly. A transcript can't read that file, so part 2 (`good-via-conflict-2.md`, run on the codebase this leaves, from this directory) loads `fixed.u`, which is what the worker would have in the file after fixing it. Worker transcripts: `delete` (stops), `load`, `update`; each extra `load` is a round of fixing errors in the file.

``` ucm :error
scratch/main> delete.term Page.size
```
