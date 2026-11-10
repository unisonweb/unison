<!-- Run from this directory (the transcript loads files next to it): cd validation && "$UNISON" -C <prepared dir> transcript.fork -S <new dir> good.md
     The upgrade cannot finish by itself, so UCM leaves the code to fix on a temporary branch. good-fix.u is the edited file (what the worker writes after reading the one UCM made); loading it and running `update` finishes. -->

``` ucm :error
scratch/main> upgrade mylib_1_0_0 mylib_2_0_0
```

``` ucm
scratch/upgrade-mylib_1_0_0-to-mylib_2_0_0> load good-fix.u
scratch/upgrade-mylib_1_0_0-to-mylib_2_0_0> update
scratch/main> list lib
scratch/main> branches
scratch/main> test
scratch/main> todo
```
