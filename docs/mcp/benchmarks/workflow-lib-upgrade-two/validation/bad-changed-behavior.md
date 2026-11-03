<!-- expected: behavior FAIL, outcome FAIL -->
<!-- Run from this directory. The agent finishes the combined upgrade but fixes `tick` with an amount of 2 and edits the tests to the new numbers, instead of keeping the behavior (bad-changed-behavior-fix.u is its edited file). -->

``` ucm :error
scratch/main> upgrade textkit_1_0_0 textkit_2_0_0 tally_1_0_0 tally_2_0_0
```

``` ucm
scratch/upgrade> load bad-changed-behavior-fix.u
scratch/upgrade> update
```
