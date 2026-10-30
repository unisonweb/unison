<!-- Run from this directory (the transcript loads files next to it): cd validation && "$UNISON" -C <prepared dir> transcript.fork -S <new dir> good.md
     One upgrade command takes both pairs. It cannot finish by itself (tally 2.0.0 changes `tick`), so UCM leaves the code to fix on a temporary branch (named `upgrade` here, because the full name would be long). good-fix.u is the edited file the worker writes after reading the one UCM made; loading it and running `update` finishes. -->

``` ucm :error
scratch/main> upgrade textkit_1_0_0 textkit_2_0_0 tally_1_0_0 tally_2_0_0
```

``` ucm
scratch/upgrade> load good-fix.u
scratch/upgrade> update
scratch/main> list lib
scratch/main> branches
scratch/main> test
scratch/main> todo
```
