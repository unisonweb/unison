<!-- expected: simultaneous FAIL, outcome PASS, behavior PASS, no-collateral PASS -->
<!-- Run from this directory. A correct end state reached in two steps: `tally` first (the file has `countThree`, `heading` and `report`; `heading` mixes `tally_2_0_0.title` with `textkit_1_0_0.shout`, so the worker has to switch it to `textkit_2_0_0.shout` by hand, one-at-a-time-fix.u), then `textkit`, which finishes by itself. The reflog shows two new entries on `main` after the setup's last `fork`. -->

``` ucm :error
scratch/main> upgrade tally_1_0_0 tally_2_0_0
```

``` ucm
scratch/upgrade-tally_1_0_0-to-tally_2_0_0> load one-at-a-time-fix.u
scratch/upgrade-tally_1_0_0-to-tally_2_0_0> update
scratch/main> upgrade textkit_1_0_0 textkit_2_0_0
scratch/main> list lib
scratch/main> branches
scratch/main> test
scratch/main> todo
scratch/main> reflog
```
