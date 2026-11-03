<!-- expected: outcome FAIL -->
The worker "removes" `Page.size` by renaming it to `Page.legacySize`: no `Page.size` and nothing breaks, but the size is still a hardcoded constant that everything uses, and nothing takes an argument.

``` ucm
scratch/main> move.term Page.size Page.legacySize
scratch/main> test
scratch/main> todo
scratch/main> branches
```
