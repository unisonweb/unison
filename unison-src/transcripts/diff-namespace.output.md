```unison
b1.x = 23
b1.fslkdjflskdjflksjdf = 663
b2.x = 23
b2.fslkdjflskdjflksjdf = 23
b2.abc = 23
```

```ucm
.> add

  ⍟ I've added these definitions:

    b1.fslkdjflskdjflksjdf : Nat
    b1.x                   : Nat
    b2.abc                 : Nat
    b2.fslkdjflskdjflksjdf : Nat
    b2.x                   : Nat

.> debug.alias.term.force b1.x b1.fslkdjflskdjflksjdf

  Done.

```
```ucm
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
junk = "asldkfjasldkfj"
```

```ucm
.ns1> add

  ⍟ I've added these definitions:

    junk : ##Text

.ns1> debug.alias.term.force junk fromJust

  Done.

.ns1> delete.term junk

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

  ⚠️

  The namespace .ns1 is empty. Was there a typo?

```

```ucm
.ns2> update.old.> diff.namespace ns1 ns2.> alias.term ns2.d ns2.d'.> alias.type ns2.A ns2.A'.> alias.type ns2.X ns2.X'.> diff.namespace ns1 ns2.> alias.type ns1.X ns1.X2.> alias.type ns2.A' ns2.A''.> fork ns2 ns3.> alias.term ns2.fromJust' ns2.yoohoo.> delete.term.verbose ns2.fromJust'.> diff.namespace ns3 ns2
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️

  The namespace .ns1 is empty. Was there a typo?
