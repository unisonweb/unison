# Implementation checks for `List.xf`

Run by `judge.sh`: it passes when the transcript completes, nothing in the output is marked as a failure, and no line of the output matches the `forbid` patterns below.

<!-- forbid: ^ +[0-9]+\. .*List\.reverse$ -->
<!-- forbid: ^ +[0-9]+\. .*List\.\+:$ -->

The tests are saved in the codebase, following the naming convention, and pass:

``` ucm
scratch/main> ls List.xf.tests
```

``` ucm
scratch/main> test List.xf.tests
```

It doesn't call `List.reverse` or `List.+:`, so it is not building the result backwards, and so is tail recursive (these two names must not be in the list):

``` ucm
scratch/main> dependencies List.xf
```

It can be called with the function first, and gives the right result, including on a long list:

``` unison
test> judge.xf.callOrder = test.verify do
  ensureEqual (List.xf (x -> x * 2) [1, 2, 3]) [2, 4, 6]

test> judge.xf.longList = test.verify do
  ensureEqual (List.size (List.xf (x -> x + 1) (List.range 0 1000000))) 1000000
```
