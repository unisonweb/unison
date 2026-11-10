<!-- expected: outcome FAIL -->
<!-- Run from this directory. The fixed file loads but the agent never runs `update`: it is left on the temporary branch and main still has the old libraries. -->

``` ucm :error
scratch/main> upgrade textkit_1_0_0 textkit_2_0_0 tally_1_0_0 tally_2_0_0
```

``` ucm
scratch/upgrade> load good-fix.u
```
