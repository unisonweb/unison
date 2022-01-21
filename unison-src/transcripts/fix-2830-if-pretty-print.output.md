Regression test for https://github.com/unisonweb/unison/pull/2830

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      multiMultiMulti    : Nat
      multiMultiSingle   : Nat
      multiSingleMulti   : Nat
      multiSingleSingle  : Nat
      singleMultiMulti   : Nat
      singleMultiSingle  : Nat
      singleSingleMulti  : Nat
      singleSingleSingle : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    multiMultiMulti    : Nat
    multiMultiSingle   : Nat
    multiSingleMulti   : Nat
    multiSingleSingle  : Nat
    singleMultiMulti   : Nat
    singleMultiSingle  : Nat
    singleSingleMulti  : Nat
    singleSingleSingle : Nat

.> view singleSingleSingle

  singleSingleSingle : Nat
  singleSingleSingle = if true then 1 else 1

.> view singleSingleMulti

  singleSingleMulti : Nat
  singleSingleMulti =
    if true then 1
    else
      1
      1

.> view singleMultiSingle

  singleMultiSingle : Nat
  singleMultiSingle =
    if true then
      1
      1
    else 1

.> view singleMultiMulti

  singleMultiMulti : Nat
  singleMultiMulti =
    if true then
      1
      1
    else
      1
      1

.> view multiSingleSingle

  multiSingleSingle : Nat
  multiSingleSingle =
    if
      true
      true
    then 1
    else 1

.> view multiSingleMulti

  multiSingleMulti : Nat
  multiSingleMulti =
    if
      true
      true
    then 1
    else
      1
      1

.> view multiMultiSingle

  multiMultiSingle : Nat
  multiMultiSingle =
    if
      true
      true
    then
      1
      1
    else 1

.> view multiMultiMulti

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
