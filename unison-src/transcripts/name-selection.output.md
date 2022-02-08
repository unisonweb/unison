This transcript shows how the pretty-printer picks names for a hash when multiple are available. The algorithm is:

1. Names that are "name-only" come before names that are hash qualified. So `List.map` comes before `List.map#2384a` and also `aaaa#xyz`.
2. Shorter names (in terms of segment count) come before longer ones, for instance `base.List.map` comes before `somelibrary.external.base.List.map`.
3. Otherwise if there are multiple names with a minimal number of segments, compare the names alphabetically.

```unison
a = b + 1
b = 0 + 1
```

Will add `a` and `b` to the codebase and give `b` a longer (in terms of segment length alias), and show that it isn't used when viewing `a`:

```ucm
  ☝️  The namespace .a is empty.

.a> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat

.a> alias.term b aaa.but.more.segments

  Done.

.a> view a

  a : Nat
  a = b + 1

```
Next let's introduce a conflicting symbol and show that its hash qualified name isn't used when it has an unconflicted name:

```
.> fork a a2
.> fork a a3

```

```unison
c = 1
d = c + 10
```

```ucm
  ☝️  The namespace .a2 is empty.

.a2> add

  ⍟ I've added these definitions:
  
    c : Nat
    d : Nat

.a2> alias.term c aaaa.tooManySegments

  Done.

```
```unison
c = 2
d = c + 10
```

```ucm
  ☝️  The namespace .a3 is empty.

.a3> add

  ⍟ I've added these definitions:
  
    c : Nat
    d : Nat

.a3> merge .a2 .a3

  Here's what's changed in .a3 after the merge:
  
  New name conflicts:
  
    1. c#dcgdua2lj6 : Nat
       ↓
    2. ┌ c#dcgdua2lj6 : Nat
    3. └ c#gjmq673r1v : Nat
    
    4. d#9ivhgvhthc : Nat
       ↓
    5. ┌ d#9ivhgvhthc : Nat
    6. └ d#ve16e6jmf6 : Nat
  
  Added definitions:
  
    7. ┌ c#gjmq673r1v         : Nat
    8. └ aaaa.tooManySegments : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
At this point, `a3` is conflicted for symbols `c` and `d`, but the original `a2` namespace has an unconflicted definition for `c` and `d`, so those are preferred.

```ucm
.> view a b c d

  a.a : Nat
  a.a = b + 1
  
  a.b : Nat
  a.b = 0 + 1
  
  a2.c : Nat
  a2.c = 1
  
  a2.d : Nat
  a2.d = a2.c + 10
  
  a3.c#dcgdua2lj6 : Nat
  a3.c#dcgdua2lj6 = 2
  
  a3.d#9ivhgvhthc : Nat
  a3.d#9ivhgvhthc = c#dcgdua2lj6 + 10

```
