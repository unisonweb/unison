```unison
x = 23
```

```ucm
  ☝️  The namespace .b1 is empty.

.b1> add

  ⍟ I've added these definitions:
  
    x : ##Nat

.b1> alias.term x fslkdjflskdjflksjdf

  Done.

.> fork b1 b2

  Done.

.b2> alias.term x abc

  Done.

```
```unison
fslkdjflskdjflksjdf = 663
```

```ucm
  ☝️  The namespace .b0 is empty.

.b0> add

  ⍟ I've added these definitions:
  
    fslkdjflskdjflksjdf : ##Nat

.> merge.old b0 b1

  Here's what's changed in b1 after the merge:
  
  New name conflicts:
  
    1. fslkdjflskdjflksjdf#u520d1t9kc : Nat
       ↓
    2. ┌ fslkdjflskdjflksjdf#sekb3fdsvb : Nat
    3. └ fslkdjflskdjflksjdf#u520d1t9kc : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

.> diff.namespace b1 b2

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

.b2> diff.namespace .b1

  Resolved name conflicts:
  
    1. ┌ fslkdjflskdjflksjdf#sekb3fdsvb : ##Nat
    2. └ fslkdjflskdjflksjdf#u520d1t9kc : ##Nat
       ↓
    3. fslkdjflskdjflksjdf#u520d1t9kc : ##Nat
  
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
  ☝️  The namespace .ns1 is empty.

.ns1> add

  ⍟ I've added these definitions:
  
    structural type A a
    structural ability X a1 a2
    b          : ##Nat
    bdependent : ##Nat
    c          : ##Nat
    fromJust   : ##Nat
    helloWorld : ##Text

.ns1> alias.term fromJust fromJust'

  Done.

.ns1> alias.term helloWorld helloWorld2

  Done.

.ns1> fork .ns1 .ns2

  Done.

```
Here's what we've done so far:

```ucm
.> diff.namespace nothing ns1

  ⚠️
  
  The namespace .nothing is empty. Was there a typo?

```
```ucm
.> diff.namespace ns1 ns2

  The namespaces are identical.

```
```unison
fromJust = "asldkfjasldkfj"
```

```ucm
  ☝️  The namespace .ns1b is empty.

.ns1b> add

  ⍟ I've added these definitions:
  
    fromJust : ##Text

.> merge.old ns1b ns1

  Here's what's changed in ns1 after the merge:
  
  New name conflicts:
  
    1. fromJust#gjmq673r1v : Nat
       ↓
    2. ┌ fromJust#gjmq673r1v : Nat
    3. └ fromJust#rnbo52q2sh : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

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
.ns2> update.old

  ⍟ I've added these definitions:
  
    type Y a b
    d : ##Nat
    e : ##Nat
    f : ##Nat
  
  ⍟ I've updated these names to your new definition:
  
    b        : ##Text
    fromJust : ##Nat
      (The old definition was also named fromJust'.)

.> diff.namespace ns1 ns2

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
    9.  Y.Y : a -> b -> Y a b
    10. d   : Nat
    11. e   : Nat
    12. f   : Nat
  
    13. patch patch (added 2 updates)

.> alias.term ns2.d ns2.d'

  Done.

.> alias.type ns2.A ns2.A'

  Done.

.> alias.type ns2.X ns2.X'

  Done.

.> diff.namespace ns1 ns2

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
    9.  Y.Y  : a -> b -> Y a b
    10. ┌ d  : Nat
    11. └ d' : Nat
    12. e    : Nat
    13. f    : Nat
  
    14. patch patch (added 2 updates)
  
  Name changes:
  
    Original  Changes
    15. A     16. A' (added)
    
    17. X    18. X' (added)

.> alias.type ns1.X ns1.X2

  Done.

.> alias.type ns2.A' ns2.A''

  Done.

.> view.patch ns2.patch

```

```ucm
.ns2> update.old.> diff.namespace ns1 ns2.> alias.term ns2.d ns2.d'.> alias.type ns2.A ns2.A'.> alias.type ns2.X ns2.X'.> diff.namespace ns1 ns2.> alias.type ns1.X ns1.X2.> alias.type ns2.A' ns2.A''.> view.patch ns2.patch.> fork ns2 ns3.> alias.term ns2.fromJust' ns2.yoohoo.> delete.term.verbose ns2.fromJust'.> diff.namespace ns3 ns2
```


🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to view.patch. Type `help` or `?` to get help.
