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

.ns1> builtins.merge

  Done.

.ns1> add

  ⍟ I've added these definitions:
  
    structural type A a
    structural ability X a1 a2
    b          : Nat
    bdependent : Nat
    c          : Nat
    fromJust   : Nat
    helloWorld : Text

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
bdependent = b
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

```ucm
.ns2> update

  Okay, I'm searching the branch for code that needs to be
  updated...

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
    
    6.  bdependent : Nat
        ↓
    7.  bdependent : Text
  
  Added definitions:
  
    8.  type Y a b
    9.  Y.Y : a -> b -> Y a b
    10. d   : Nat
    11. e   : Nat
    12. f   : Nat
  
  Name changes:
  
    Original                   Changes
    13. fromJust'           ┐  14. fromJust#gjmq673r1v (removed)
    15. fromJust#gjmq673r1v ┘  

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
    
    6.  bdependent : Nat
        ↓
    7.  bdependent : Text
  
  Added definitions:
  
    8.  type Y a b
    9.  Y.Y  : a -> b -> Y a b
    10. ┌ d  : Nat
    11. └ d' : Nat
    12. e    : Nat
    13. f    : Nat
  
  Name changes:
  
    Original                   Changes
    14. A                      15. A' (added)
    
    16. X                      17. X' (added)
    
    18. fromJust'           ┐  19. fromJust#gjmq673r1v (removed)
    20. fromJust#gjmq673r1v ┘  

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
  
    Original                      Changes
    1. ns1.fromJust'           ┐  2. ns2.fromJust' (removed)
    3. ns2.fromJust'           │  
    4. ns2.yoohoo              │  
    5. ns3.fromJust'           │  
    6. ns1.fromJust#gjmq673r1v ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> diff.namespace ns3 ns2

  Name changes:
  
    Original        Changes
    1. fromJust'    2. yoohoo (added)
                    3. fromJust' (removed)

```
```unison
bdependent = "banana"
```

```ucm
.ns3> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

.> diff.namespace ns2 ns3

  Updates:
  
    1. bdependent : Text
       ↓
    2. bdependent : Text
  
  Name changes:
  
    Original     Changes
    3. yoohoo    4. fromJust' (added)
                 5. yoohoo (removed)

```
## Should be able to diff a namespace hash from history.

```unison
x = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat
        (also named ns1.fromJust , ns1.fromJust' , ns3.fromJust'
        , and ns2.yoohoo)

```
```ucm
  ☝️  The namespace .hashdiff is empty.

.hashdiff> add

  ⍟ I've added these definitions:
  
    x : ##Nat

```
```unison
y = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : ##Nat

```
```ucm
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

  1. foo#jk19sm5bf8 : Nat - do we want to force a hashqualified? Arya thinks so
     ↓
  2. ┌ foo#0ja1qfpej6 : Nat
  3. └ foo#jk19sm5bf8 : Nat

Resolved name conflicts: -- updates where LHS had multiple hashes and RHS has one

  4. ┌ bar#0ja1qfpej6 : Nat
  5. └ bar#jk19sm5bf8 : Nat
     ↓
  6. bar#jk19sm5bf8 : Nat

## Display issues to fixup

- [d] Do we want to surface new edit conflicts in patches?
- [t] two different auto-propagated changes creating a name conflict should show
      up somewhere besides the auto-propagate count
- [t] Things look screwy when the type signature doesn't fit and has to get broken
      up into multiple lines. Maybe just disallow that?
- [d] Delete blank line in between copies / renames entries if all entries are 1 to 1
      see todo in the code
- [x] incorrectly calculated bracket alignment on hashqualified "Name changes"  (delete.output.md)
- [x] just handle deletion of isPropagated in propagate function, leave HandleInput alone (assuming this does the trick)
- [x] might want unqualified names to be qualified sometimes:
- [x] if a name is updated to a not-yet-named reference, it's shown as both an update and an add
- [x] similarly, if a conflicted name is resolved by deleting the last name to
      a reference, I (arya) suspect it will show up as a Remove
- [d] Maybe group and/or add headings to the types, constructors, terms
- [x] add tagging of propagated updates to test propagated updates output
- [x] missing old names in deletion ppe (delete.output.md)  (superseded by \#1143)
- [x] delete.term has some bonkers output
- [x] Make a decision about how we want to show constructors in the diff
- [x] 12.patch patch needs a space
- [x] This looks like garbage
- [x] Extra 2 blank lines at the end of the add section
- [x] Fix alignment issues with buildTable, convert to column3M (to be written)
- [x] adding an alias is showing up as an Add and a Copy; should just show as Copy
- [x] removing one of multiple aliases appears in removes + moves + copies section
- [x] some overlapping cases between Moves and Copies^
- [x] Maybe don't list the type signature twice for aliases?
