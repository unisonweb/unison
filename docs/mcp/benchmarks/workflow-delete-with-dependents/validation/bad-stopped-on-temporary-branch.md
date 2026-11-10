<!-- expected: outcome FAIL -->
The worker deletes `Page.size`, sees that UCM can't finish, and stops: the codebase is left on `update-main`, `Page.size` is still in `main` and nothing was fixed.

``` ucm :error
scratch/main> delete.term Page.size
```
