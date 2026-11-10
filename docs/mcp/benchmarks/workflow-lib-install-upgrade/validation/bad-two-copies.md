<!-- expected: outcome FAIL -->
<!-- Needs the network. The agent installs the new release and stops: both copies are in lib and parseNats still uses the old one. -->

``` ucm
scratch/main> lib.install @unison/json/releases/latest
scratch/main> test
```
