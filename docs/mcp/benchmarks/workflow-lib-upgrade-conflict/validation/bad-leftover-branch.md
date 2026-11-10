<!-- expected: outcome FAIL -->
<!-- Run from this directory. The fixed file loads but the agent never runs `update`, so the project is left on the temporary branch with the old code on main. -->

``` ucm :error
scratch/main> upgrade mylib_1_0_0 mylib_2_0_0
```

``` ucm
scratch/upgrade-mylib_1_0_0-to-mylib_2_0_0> load good-fix.u
```
