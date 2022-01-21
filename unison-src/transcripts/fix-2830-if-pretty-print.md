Regression test for https://github.com/unisonweb/unison/pull/2830

```ucm:hide
.> builtins.merge
```

To check that `if`s roundtrip, there are 8 cases to test: whether the condition,
true branch, and false branch are single-line or multiline.

```unison
singleSingleSingle : Nat
singleSingleSingle = if true then 1 else 1

singleSingleMulti : Nat
singleSingleMulti =
  if
    true
  then
    1
  else
    1
    1

singleMultiSingle : Nat
singleMultiSingle =
  if
    true
  then
    1
    1
  else
    1

singleMultiMulti : Nat
singleMultiMulti =
  if true then
    1
    1
  else
    1
    1

multiSingleSingle : Nat
multiSingleSingle =
  if
    true
    true
  then
    1
  else
    1

multiSingleMulti : Nat
multiSingleMulti =
  if
    true
    true
  then
    1
  else
    1
    1

multiMultiSingle : Nat
multiMultiSingle =
  if
    true
    true
  then
    1
    1
  else
    1

multiMultiMulti : Nat
multiMultiMulti =
  if
    true
    true
  then
    1
    1
  else
    1
    1
```

```ucm
.> add
.> view singleSingleSingle
.> view singleSingleMulti
.> view singleMultiSingle
.> view singleMultiMulti
.> view multiSingleSingle
.> view multiSingleMulti
.> view multiMultiSingle
.> view multiMultiMulti
```

