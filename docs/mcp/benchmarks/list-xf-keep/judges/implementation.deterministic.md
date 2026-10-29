# Implementation checks for `List.xfKeep`

Run by `judge.sh`: it passes when the transcript completes, nothing in the output is marked as a failure, and no line of the output matches the `forbid` patterns below.

<!-- forbid: ^ +[0-9]+\. .*List\.reverse$ -->
<!-- forbid: ^ +[0-9]+\. .*List\.\+:$ -->

The tests are saved in the codebase, following the naming convention, and pass:

``` ucm
scratch/main> ls List.xfKeep.tests
```

``` ucm
scratch/main> test List.xfKeep.tests
```

It doesn't call `List.reverse` or `List.+:`, so it is not building the result backwards, and so is tail recursive (these two names must not be in the list):

``` ucm
scratch/main> dependencies List.xfKeep
```

It can be called with the function first, and gives the right result, including on a long list:

``` unison
test> judge.xfKeep.callOrder = test.verify do
  ensureEqual (List.xfKeep (x -> if x > 1 then Some (x * 10) else None) [1, 2, 3]) [20, 30]

test> judge.xfKeep.longList = test.verify do
  ensureEqual (List.size (List.xfKeep (x -> if Nat.isEven x then Some x else None) (List.range 0 1000000))) 500000
```
