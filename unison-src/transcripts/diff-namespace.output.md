```unison
x = 23
fslkdjflskdjflksjdf = 663
```

```ucm
scratch/b1> add

  ⍟ I've added these definitions:
  
    fslkdjflskdjflksjdf : Nat
    x                   : Nat

```
```unison
x = 23
fslkdjflskdjflksjdf = 23
abc = 23
```

```ucm
scratch/b2> add

  ⍟ I've added these definitions:
  
    abc                 : Nat
    fslkdjflskdjflksjdf : Nat
    x                   : Nat

scratch/b1> debug.alias.term.force .x .fslkdjflskdjflksjdf

  Done.

```
```ucm
scratch/main> diff.namespace /b1: /b2:

  Resolved name conflicts:
  
    1. ┌ fslkdjflskdjflksjdf#sekb3fdsvb : Nat
    2. └ fslkdjflskdjflksjdf#u520d1t9kc : Nat
       ↓
    3. fslkdjflskdjflksjdf#u520d1t9kc : Nat
  
  Name changes:
  
    Original                             Changes
    4. x                              ┐  5. abc (added)
    6. fslkdjflskdjflksjdf#u520d1t9kc ┘  7. fslkdjflskdjflksjdf (added)
                                         8. fslkdjflskdjflksjdf#u520d1t9kc (removed)

```
Things we want to test:

* Diffing identical namespaces
* Adds, removes, updates
  * Adds with multiple names
* Moved and copied definitions
  * Moves that have more that 1 initial or final name
* ... terms and types
* New patches, modified patches, deleted patches, moved patches
* With and without propagated updates

```unison
fromJust = 1
b = 2
bdependent = b
c = 3
helloWorld = "Hello, world!"

structural type A a = A ()
structural ability X a1 a2 where x : ()
```

```ucm
scratch/ns1> add

  ⍟ I've added these definitions:
  
    structural type A a
    structural ability X a1 a2
    b          : ##Nat
    bdependent : ##Nat
    c          : ##Nat
    fromJust   : ##Nat
    helloWorld : ##Text

scratch/ns1> alias.term fromJust fromJust'

  Done.

scratch/ns1> alias.term helloWorld helloWorld2

  Done.

scratch/ns1> branch /ns2

  Done. I've created the ns2 branch based off of ns1.
  
  Tip: To merge your work back into the ns1 branch, first
       `switch /ns1` then `merge /ns2`.

```
Here's what we've done so far:

```ucm
scratch/main> diff.namespace .nothing /ns1:

  ⚠️
  
  The namespace scratch/main:.nothing is empty. Was there a typo?

```
```ucm
scratch/main> diff.namespace /ns1: ns2:

<none>:1:4:
  |
1 | ns2:
  |    ^
unexpected ':'
expecting '/' or end of input


```
```unison
junk = "asldkfjasldkfj"
```

```ucm
scratch/ns1> add

  ⍟ I've added these definitions:
  
    junk : ##Text

scratch/ns1> debug.alias.term.force junk fromJust

  Done.

scratch/ns1> delete.term junk

  Done.

```
```unison
fromJust = 99
b = "oog"
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

```ucm
scratch/ns2> update.old

  ⍟ I've added these definitions:
  
    type Y a b
    d : ##Nat
    e : ##Nat
    f : ##Nat
  
  ⍟ I've updated these names to your new definition:
  
    b        : ##Text
    fromJust : ##Nat
      (The old definition was also named fromJust'.)

scratch/main> diff.namespace /ns1: /ns2:

  Resolved name conflicts:
  
    1.  ┌ fromJust#gjmq673r1v : Nat
    2.  └ fromJust#rnbo52q2sh : Text
        ↓
    3.  fromJust#6gn1k53ie0 : Nat
  
  Updates:
  
    4.  b : Nat
        ↓
    5.  b : Text
    
    6.  fromJust' : Nat
        ↓
    7.  fromJust' : Nat
  
  Added definitions:
  
    8.  type Y a b
    9.  Y.Y : a -> b -> #md85ksgqel a b
    10. d   : Nat
    11. e   : Nat
    12. f   : Nat
  
    13. patch patch (added 2 updates)

scratch/ns2> alias.term d d'

  Done.

scratch/ns2> alias.type A A'

  Done.

scratch/ns2> alias.type X X'

  Done.

scratch/main> diff.namespace /ns1: /ns2:

  Resolved name conflicts:
  
    1.  ┌ fromJust#gjmq673r1v : Nat
    2.  └ fromJust#rnbo52q2sh : Text
        ↓
    3.  fromJust#6gn1k53ie0 : Nat
  
  Updates:
  
    4.  b : Nat
        ↓
    5.  b : Text
    
    6.  fromJust' : Nat
        ↓
    7.  fromJust' : Nat
  
  Added definitions:
  
    8.  type Y a b
    9.  Y.Y  : a -> b -> #md85ksgqel a b
    10. ┌ d  : Nat
    11. └ d' : Nat
    12. e    : Nat
    13. f    : Nat
  
    14. patch patch (added 2 updates)
  
  Name changes:
  
    Original  Changes
    15. A     16. A' (added)
    
    17. X    18. X' (added)

scratch/ns1> alias.type X X2

  Done.

scratch/ns2> alias.type A' A''

  Done.

scratch/ns2> branch /ns3

  Done. I've created the ns3 branch based off of ns2.
  
  Tip: To merge your work back into the ns2 branch, first
       `switch /ns2` then `merge /ns3`.

scratch/ns2> alias.term fromJust' yoohoo

  Done.

scratch/ns2> delete.term.verbose fromJust'

  Name changes:
  
    Original        Changes
    1. fromJust  ┐  2. fromJust' (removed)
    3. fromJust' │  
    4. yoohoo    ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

scratch/main> diff.namespace /ns3: /ns2:

  Name changes:
  
    Original        Changes
    1. fromJust  ┐  2. yoohoo (added)
    3. fromJust' ┘  4. fromJust' (removed)

```
```unison
bdependent = "banana"
```

```ucm
scratch/ns3> update.old

  ⍟ I've updated these names to your new definition:
  
    bdependent : ##Text

scratch/main> diff.namespace /ns2: /ns3:

  Updates:
  
    1. bdependent : Nat
       ↓
    2. bdependent : Text
  
    3. patch patch (added 1 updates)
  
  Name changes:
  
    Original       Changes
    4. fromJust ┐  5. fromJust' (added)
    6. yoohoo   ┘  7. yoohoo (removed)

```
## Two different auto-propagated changes creating a name conflict

Currently, the auto-propagated name-conflicted definitions are not explicitly
shown, only their also-conflicted dependency is shown.

```unison
a = 333
b = a + 1
```

```ucm
scratch/nsx> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat

scratch/nsx> branch /nsy

  Done. I've created the nsy branch based off of nsx.
  
  Tip: To merge your work back into the nsx branch, first
       `switch /nsx` then `merge /nsy`.

scratch/nsx> branch /nsz

  Done. I've created the nsz branch based off of nsx.
  
  Tip: To merge your work back into the nsx branch, first
       `switch /nsx` then `merge /nsz`.

```
```unison
a = 444
```

```ucm
scratch/nsy> update.old

  ⍟ I've updated these names to your new definition:
  
    a : Nat

```
```unison
a = 555
```

```ucm
scratch/nsz> update.old

  ⍟ I've updated these names to your new definition:
  
    a : Nat

scratch/nsy> branch /nsw

  Done. I've created the nsw branch based off of nsy.
  
  Tip: To merge your work back into the nsy branch, first
       `switch /nsy` then `merge /nsw`.

scratch/main> debug.alias.term.force /nsz:.a /nsw:.a

```

```ucm
scratch/nsz> update.oldscratch/nsy> branch /nswscratch/main> debug.alias.term.force /nsz:.a /nsw:.ascratch/main> debug.alias.term.force /nsz:.b /nsw:.b
```


🛑

The transcript failed due to an error in the stanza above. The error is:

1:2:
  |
1 | /nsz:.a
  |  ^
unexpected 'n'
expecting '.', end of input, hash (ex: #af3sj3), or operator (valid characters: !$%&*+-/:<=>\^|~)

