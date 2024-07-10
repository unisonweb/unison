``` unison
b1.x = 23
b1.fslkdjflskdjflksjdf = 663
b2.x = 23
b2.fslkdjflskdjflksjdf = 23
b2.abc = 23
```

``` ucm
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
``` ucm
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

  - Diffing identical namespaces
  - Adds, removes, updates
      - Adds with multiple names
  - Moved and copied definitions
      - Moves that have more that 1 initial or final name
  - ... terms and types
  - New patches, modified patches, deleted patches, moved patches
  - With and without propagated updates

``` unison
fromJust = 1
b = 2
bdependent = b
c = 3
helloWorld = "Hello, world!"

structural type A a = A ()
structural ability X a1 a2 where x : ()
```

``` ucm
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

``` ucm
.> diff.namespace nothing ns1

  ⚠️
  
  The namespace .nothing is empty. Was there a typo?

```
``` ucm
.> diff.namespace ns1 ns2

  The namespaces are identical.

```
``` unison
junk = "asldkfjasldkfj"
```

``` ucm
.ns1> add

  ⍟ I've added these definitions:
  
    junk : ##Text

.ns1> debug.alias.term.force junk fromJust

  Done.

.ns1> delete.term junk

  Done.

```
``` unison
fromJust = 99
b = "oog"
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

``` ucm
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

.> fork ns2 ns3

  Done.

.> alias.term ns2.fromJust' ns2.yoohoo

  Done.

.> delete.term.verbose ns2.fromJust'

  Name changes:
  
    Original            Changes
    1. ns2.fromJust  ┐  2. ns2.fromJust' (removed)
    3. ns2.fromJust' │  
    4. ns2.yoohoo    │  
    5. ns3.fromJust  │  
    6. ns3.fromJust' ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> diff.namespace ns3 ns2

  Name changes:
  
    Original        Changes
    1. fromJust  ┐  2. yoohoo (added)
    3. fromJust' ┘  4. fromJust' (removed)

```
``` unison
bdependent = "banana"
```

``` ucm
.ns3> update.old

  ⍟ I've updated these names to your new definition:
  
    bdependent : ##Text

.> diff.namespace ns2 ns3

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

``` unison
a = 333
b = a + 1
```

``` ucm
  ☝️  The namespace .nsx is empty.

.nsx> add

  ⍟ I've added these definitions:
  
    a : ##Nat
    b : ##Nat

.> fork nsx nsy

  Done.

.> fork nsx nsz

  Done.

```
``` unison
a = 444
```

``` ucm
.nsy> update.old

  ⍟ I've updated these names to your new definition:
  
    a : ##Nat

```
``` unison
a = 555
```

``` ucm
.nsz> update.old

  ⍟ I've updated these names to your new definition:
  
    a : ##Nat

.> fork nsy nsw

  Done.

.> debug.alias.term.force nsz.a nsw.a

  Done.

.> debug.alias.term.force nsz.b nsw.b

  Done.

```
``` ucm
.> diff.namespace nsx nsw

  New name conflicts:
  
    1. a#uiiiv8a86s : Nat
       ↓
    2. ┌ a#mdl4vqtu00 : Nat
    3. └ a#vrs8gtkl2t : Nat
    
    4. b#lhigeb1let : Nat
       ↓
    5. ┌ b#aapqletas7 : Nat
    6. └ b#unkqhuu66p : Nat
  
  Added definitions:
  
    7. patch patch (added 1 updates)

.nsw> view a b

  a#mdl4vqtu00 : ##Nat
  a#mdl4vqtu00 = 444
  
  a#vrs8gtkl2t : ##Nat
  a#vrs8gtkl2t = 555
  
  b#aapqletas7 : ##Nat
  b#aapqletas7 = ##Nat.+ a#vrs8gtkl2t 1
  
  b#unkqhuu66p : ##Nat
  b#unkqhuu66p = ##Nat.+ a#mdl4vqtu00 1

```
## Should be able to diff a namespace hash from history.

``` unison
x = 1
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : ##Nat

```
``` ucm
  ☝️  The namespace .hashdiff is empty.

.hashdiff> add

  ⍟ I've added these definitions:
  
    x : ##Nat

```
``` unison
y = 2
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : ##Nat

```
``` ucm
.hashdiff> add

  ⍟ I've added these definitions:
  
    y : ##Nat

.hashdiff> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #ru1hnjofdj
  
    + Adds / updates:
    
      y
  
  □ 2. #i52j9fd57b (start of history)

.hashdiff> diff.namespace 2 1

  Added definitions:
  
    1. y : ##Nat

```
## 

Updates:  -- 1 to 1

New name conflicts: -- updates where RHS has multiple hashes (excluding when RHS=LHS)

1.  foo\#jk19sm5bf8 : Nat - do we want to force a hashqualified? Arya thinks so
    ↓
2.  ┌ foo\#0ja1qfpej6 : Nat
3.  └ foo\#jk19sm5bf8 : Nat

Resolved name conflicts: -- updates where LHS had multiple hashes and RHS has one

4.  ┌ bar\#0ja1qfpej6 : Nat
5.  └ bar\#jk19sm5bf8 : Nat
    ↓
6.  bar\#jk19sm5bf8 : Nat

## Display issues to fixup

  - \[d\] Do we want to surface new edit conflicts in patches?
  - \[t\] two different auto-propagated changes creating a name conflict should show
    up somewhere besides the auto-propagate count
  - \[t\] Things look screwy when the type signature doesn't fit and has to get broken
    up into multiple lines. Maybe just disallow that?
  - \[d\] Delete blank line in between copies / renames entries if all entries are 1 to 1
    see todo in the code
  - \[x\] incorrectly calculated bracket alignment on hashqualified "Name changes"  (delete.output.md)
  - \[x\] just handle deletion of isPropagated in propagate function, leave HandleInput alone (assuming this does the trick)
  - \[x\] might want unqualified names to be qualified sometimes:
  - \[x\] if a name is updated to a not-yet-named reference, it's shown as both an update and an add
  - \[x\] similarly, if a conflicted name is resolved by deleting the last name to
    a reference, I (arya) suspect it will show up as a Remove
  - \[d\] Maybe group and/or add headings to the types, constructors, terms
  - \[x\] add tagging of propagated updates to test propagated updates output
  - \[x\] missing old names in deletion ppe (delete.output.md)  (superseded by \#1143)
  - \[x\] delete.term has some bonkers output
  - \[x\] Make a decision about how we want to show constructors in the diff
  - \[x\] 12.patch patch needs a space
  - \[x\] This looks like garbage
  - \[x\] Extra 2 blank lines at the end of the add section
  - \[x\] Fix alignment issues with buildTable, convert to column3M (to be written)
  - \[x\] adding an alias is showing up as an Add and a Copy; should just show as Copy
  - \[x\] removing one of multiple aliases appears in removes + moves + copies section
  - \[x\] some overlapping cases between Moves and Copies^
  - \[x\] Maybe don't list the type signature twice for aliases?

