<!-- expected: outcome FAIL (simultaneous FAIL too) -->
<!-- Run from this directory. The agent upgrades only the pair that breaks code, fixes it (bad-one-pair-fix.u), and forgets `textkit`: textkit_1_0_0 stays in lib and `excite` still uses it. -->

``` ucm :error
scratch/main> upgrade tally_1_0_0 tally_2_0_0
```

``` ucm
scratch/upgrade-tally_1_0_0-to-tally_2_0_0> load bad-one-pair-fix.u
scratch/upgrade-tally_1_0_0-to-tally_2_0_0> update
scratch/main> list lib
```
