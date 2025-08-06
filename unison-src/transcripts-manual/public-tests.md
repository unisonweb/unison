Runs tests on the latest releases of some public projects.

It turns out that pulling releases is very fast compared to actually running the tests (which take about 10 minutes total), so it's not worth caching the codebase.

```ucm
x/base> pull @unison/base/releases/latest
> test

x/json> pull @unison/json/releases/latest
> test

x/cloud> pull @unison/cloud/releases/latest
> test
> io.test internal.tests.cloud.runAllLocally

x/orderator> pull @pchiusano/orderator/releases/latest
> test
```
