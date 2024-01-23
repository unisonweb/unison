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
.a> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat

.a> alias.term b aaa.but.more.segments

  Done.

.a> view a

  a : Nat
  a =
    use Nat +
    b + 1

.> cd .

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
.a2> add

  ⍟ I've added these definitions:
  
    c : Nat
    d : Nat

.a2> alias.term c long.name.but.shortest.suffixification

  Done.

```
```unison
c = 2
d = c + 10
```

```ucm
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
  
    7. ┌ c#gjmq673r1v                           : Nat
    8. └ long.name.but.shortest.suffixification : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

```
At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized. 
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope, 
`a2.c` is chosen because although the suffixified version has fewer segments, its fully-qualified name has the fewest segments.

```ucm
.> view a b c d

  a.a : Nat
  a.a =
    use Nat +
    b + 1
  
  a.b : Nat
  a.b =
    use Nat +
    0 + 1
  
  a2.c : Nat
  a2.c = 1
  
  a2.d : Nat
  a2.d =
    use Nat +
    a2.c + 10
  
  a3.c#dcgdua2lj6 : Nat
  a3.c#dcgdua2lj6 = 2
  
  a3.d#9ivhgvhthc : Nat
  a3.d#9ivhgvhthc =
    use Nat +
    c#dcgdua2lj6 + 10

```
## Name biasing

```unison
deeply.nested.term = 
  a + 1

deeply.nested.num = 10

a = 10
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a                  : Nat
      deeply.nested.num  : Nat
      deeply.nested.term : Nat

```
```ucm
.biasing> add

  ⍟ I've added these definitions:
  
    a                  : Nat
    deeply.nested.num  : Nat
    deeply.nested.term : Nat

-- Despite being saved with name `a`, 
-- the pretty printer should prefer the suffixified 'deeply.nested.num name' over the shallow 'a'.
-- It's closer to the term being printed.
.biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    num + 1

```
Add another term with `num` suffix to force longer suffixification of `deeply.nested.num`

```unison
other.num = 20
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      other.num : Nat

```
```ucm
.biasing> add

  ⍟ I've added these definitions:
  
    other.num : Nat

-- nested.num should be preferred over the shorter name `a` due to biasing
-- because `deeply.nested.num` is nearby to the term being viewed.
.biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    nested.num + 1

```
