```ucm
.> builtins.merge

  Done.

```
```unison
---
title: /private/tmp/scratch.u
---
foo = 123

bar = 456

mytest = [Ok "ok"]

```


```ucm

  Loading changes detected in /private/tmp/scratch.u.

  I found and typechecked these definitions in
  /private/tmp/scratch.u. If you do an `add` or `update`, here's
  how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar    : Nat
      foo    : Nat
      mytest : [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    bar    : Nat
    foo    : Nat
    mytest : [Result]

.> edit foo bar

  ☝️
  
  I added 2 definitions to the top of /private/tmp/scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

.> edit mytest

  ☝️
  
  I added 1 definitions to the top of /private/tmp/scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
```unison:added-by-ucm /private/tmp/scratch.u
test> mytest = [Ok "ok"]
```

```unison:added-by-ucm /private/tmp/scratch.u
bar : Nat
bar = 456

foo : Nat
foo = 123
```

```ucm
.> edit missing

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    missing

```
