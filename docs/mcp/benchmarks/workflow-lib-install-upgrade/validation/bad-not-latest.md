<!-- expected: outcome FAIL -->
<!-- Needs the network. The agent upgrades cleanly, but to 1.4.1, an older release than the latest. -->

``` ucm
scratch/main> lib.install @unison/json/releases/1.4.1
scratch/main> upgrade unison_json_1_2_1 unison_json_1_4_1
scratch/main> test
```
