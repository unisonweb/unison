<!-- expected: outcome FAIL -->
<!-- Needs the network. The agent installs the new release, then deletes the old folder by hand instead of upgrading: parseNats no longer has its dependency. -->

``` ucm
scratch/main> lib.install @unison/json/releases/latest
scratch/main> delete.namespace.force lib.unison_json_1_2_1
scratch/main> todo
```
