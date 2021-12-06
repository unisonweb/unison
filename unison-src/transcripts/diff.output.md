```unison
x = 23
```

```ucm
  ☝️  The namespace .b1 is empty.

.b1> add

  ⍟ I've added these definitions:
  
    x : Nat

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
  
    fslkdjflskdjflksjdf : Nat

.> merge b0 b1

  Here's what's changed in b1 after the merge:
  
  New name conflicts:
  
    1. fslkdjflskdjflksjdf#4vn50bh8pk : Nat
       ↓
    2. ┌ fslkdjflskdjflksjdf#4vn50bh8pk : Nat
    3. └ fslkdjflskdjflksjdf#9mupj24g1n : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> diff.namespace b1 b2

  Resolved name conflicts:
  
    1. ┌ fslkdjflskdjflksjdf#4vn50bh8pk : Nat
    2. └ fslkdjflskdjflksjdf#9mupj24g1n : Nat
       ↓
    3. fslkdjflskdjflksjdf#4vn50bh8pk : Nat
  
  Name changes:
  
    Original                             Changes
    4. x                              ┐  5. abc (added)
    6. fslkdjflskdjflksjdf#4vn50bh8pk ┘  7. fslkdjflskdjflksjdf (added)
                                         8. fslkdjflskdjflksjdf#4vn50bh8pk (removed)

.b2> diff.namespace .b1

  Resolved name conflicts:
  
    1. ┌ fslkdjflskdjflksjdf#4vn50bh8pk : Nat
    2. └ fslkdjflskdjflksjdf#9mupj24g1n : Nat
       ↓
    3. fslkdjflskdjflksjdf#4vn50bh8pk : Nat
  
  Name changes:
  
    Original                             Changes
    4. x                              ┐  5. abc (added)
    6. fslkdjflskdjflksjdf#4vn50bh8pk ┘  7. fslkdjflskdjflksjdf (added)
                                         8. fslkdjflskdjflksjdf#4vn50bh8pk (removed)

```
Things we want to test:

* Diffing identical namespaces
* Adds, removes, updates (with and without metadata updates)
  * Adds with multiple names
  * Adds with multiple names and different metadata on each
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

structural type A a = A Nat
structural ability X a1 a2 where x : Nat
```

```ucm
  ☝️  The namespace .ns1 is empty.

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

.ns1> link b fromJust

  Updates:
  
    1. ns1.fromJust : Nat
       + 2. b : Nat
    
    3. ns1.fromJust' : Nat
       + 4. b : Nat

.ns1> fork .ns1 .ns2

  Done.

.ns1> cd .

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
  
    fromJust : Text

.> merge ns1b ns1

  Here's what's changed in ns1 after the merge:
  
  New name conflicts:
  
    1. fromJust#rrsqv1ogaq : Nat
       ↓
    2. ┌ fromJust#8vv2a5jnig : Text
    3. └ fromJust#rrsqv1ogaq : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

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
.ns2> update

  ⍟ I've added these definitions:
  
    unique type Y a b
    d : Nat
    e : Nat
    f : Nat
  
  ⍟ I've updated these names to your new definition:
  
    b        : Text
    fromJust : Nat
      (The old definition was also named fromJust'. I updated
      this name too.)

.ns2> links fromJust

  1. b : Text
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> diff.namespace ns1 ns2

  Resolved name conflicts:
  
    1.  ┌ fromJust#8vv2a5jnig : Text
    2.  └ fromJust#rrsqv1ogaq : Nat
        ↓
    3.  fromJust#mkj3tehhkv : Nat
        - 4.  ns1.b : Nat
        + 5.  ns2.b : Text
  
  Updates:
  
    6.  b : Nat
        ↓
    7.  b : Text
    
    8.  fromJust' : Nat
        ↓
    9.  fromJust' : Nat
        - 10. ns1.b : Nat
        + 11. ns2.b : Text
  
  Added definitions:
  
    12. unique type Y a b
    13. Y.Y : a -> b -> Y a b
    14. d   : Nat
    15. e   : Nat
    16. f   : Nat
  
    17. patch patch (added 2 updates)

.> alias.term ns2.d ns2.d'

  Done.

.> alias.type ns2.A ns2.A'

  Done.

.> alias.type ns2.X ns2.X'

  Done.

.> diff.namespace ns1 ns2

  Resolved name conflicts:
  
    1.  ┌ fromJust#8vv2a5jnig : Text
    2.  └ fromJust#rrsqv1ogaq : Nat
        ↓
    3.  fromJust#mkj3tehhkv : Nat
        - 4.  ns1.b : Nat
        + 5.  ns2.b : Text
  
  Updates:
  
    6.  b : Nat
        ↓
    7.  b : Text
    
    8.  fromJust' : Nat
        ↓
    9.  fromJust' : Nat
        - 10. ns1.b : Nat
        + 11. ns2.b : Text
  
  Added definitions:
  
    12. unique type Y a b
    13. Y.Y  : a -> b -> Y a b
    14. ┌ d  : Nat
    15. └ d' : Nat
    16. e    : Nat
    17. f    : Nat
  
    18. patch patch (added 2 updates)
  
  Name changes:
  
    Original  Changes
    19. A     20. A' (added)
    
    21. X    22. X' (added)

.> link ns1.c ns2.f

  Updates:
  
    1. ns2.f : Nat
       + 2. c : Nat

.> link ns2.c ns2.c

  Updates:
  
    1. ns2.c : Nat
       + 2. c : Nat

.> diff.namespace ns1 ns2

  Resolved name conflicts:
  
    1.  ┌ fromJust#8vv2a5jnig : Text
    2.  └ fromJust#rrsqv1ogaq : Nat
        ↓
    3.  fromJust#mkj3tehhkv : Nat
        - 4.  ns1.b : Nat
        + 5.  ns2.b : Text
  
  Updates:
  
    6.  b : Nat
        ↓
    7.  b : Text
    
    8.  c : Nat
        + 9.  c : Nat
    
    10. fromJust' : Nat
        ↓
    11. fromJust' : Nat
        - 12. ns1.b : Nat
        + 13. ns2.b : Text
  
  Added definitions:
  
    14. unique type Y a b
    15. Y.Y  : a -> b -> Y a b
    16. ┌ d  : Nat
    17. └ d' : Nat
    18. e    : Nat
    19. f    : Nat (+1 metadata)
  
    20. patch patch (added 2 updates)
  
  Name changes:
  
    Original  Changes
    21. A     22. A' (added)
    
    23. X    24. X' (added)

.> unlink ns2.b ns2.fromJust

  I didn't make any changes.

.> diff.namespace ns1 ns2

  Resolved name conflicts:
  
    1.  ┌ fromJust#8vv2a5jnig : Text
    2.  └ fromJust#rrsqv1ogaq : Nat
        ↓
    3.  fromJust#mkj3tehhkv : Nat
        - 4.  ns1.b : Nat
        + 5.  ns2.b : Text
  
  Updates:
  
    6.  b : Nat
        ↓
    7.  b : Text
    
    8.  c : Nat
        + 9.  c : Nat
    
    10. fromJust' : Nat
        ↓
    11. fromJust' : Nat
        - 12. ns1.b : Nat
        + 13. ns2.b : Text
  
  Added definitions:
  
    14. unique type Y a b
    15. Y.Y  : a -> b -> Y a b
    16. ┌ d  : Nat
    17. └ d' : Nat
    18. e    : Nat
    19. f    : Nat (+1 metadata)
  
    20. patch patch (added 2 updates)
  
  Name changes:
  
    Original  Changes
    21. A     22. A' (added)
    
    23. X    24. X' (added)

.> alias.type ns1.X ns1.X2

  Done.

.> alias.type ns2.A' ns2.A''

  Done.

.> view.patch ns2.patch

  Edited Terms:
    ns1.b         -> ns2.b
    ns1.fromJust' -> ns2.fromJust
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> fork ns2 ns3

  Done.

.> alias.term ns2.fromJust' ns2.yoohoo

  Done.

.> delete.term ns2.fromJust'

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
```unison
bdependent = "banana"
```

```ucm
.ns3> update

  ⍟ I've updated these names to your new definition:
  
    bdependent : Text

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
```unison
a = 333
b = a + 1
```

```ucm
  ☝️  The namespace .nsx is empty.

.nsx> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat

.> fork nsx nsy

  Done.

.> fork nsx nsz

  Done.

```
```unison
a = 444
```

```ucm
.nsy> update

  ⍟ I've updated these names to your new definition:
  
    a : Nat

```
```unison
a = 555
```

```ucm
.nsz> update

  ⍟ I've updated these names to your new definition:
  
    a : Nat

.> merge nsy nsw

  Here's what's changed in nsw after the merge:
  
  Added definitions:
  
    1. a : Nat
    2. b : Nat (+1 metadata)
  
    3. patch patch (added 1 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.> merge nsz nsw

  Here's what's changed in nsw after the merge:
  
  New name conflicts:
  
    1. a#er8q007v02 : Nat
       ↓
    2. ┌ a#75jrr0qj1c : Nat
    3. └ a#er8q007v02 : Nat
  
  Updates:
  
    4. b#j6aq9c9k9v : Nat
  
    There were 1 auto-propagated updates.
  
    5. patch patch (added 1 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  I tried to auto-apply the patch, but couldn't because it
  contained contradictory entries.

```
```ucm
.> diff.namespace nsx nsw

  New name conflicts:
  
    1. a#nfb8lmj46i : Nat
       ↓
    2. ┌ a#75jrr0qj1c : Nat
    3. └ a#er8q007v02 : Nat
  
  Updates:
  
    There were 2 auto-propagated updates.
  
  Added definitions:
  
    4. patch patch (added 2 updates)

.nsw> view a b

  a#75jrr0qj1c : Nat
  a#75jrr0qj1c = 555
  
  a#er8q007v02 : Nat
  a#er8q007v02 = 444
  
  b#j6aq9c9k9v : Nat
  b#j6aq9c9k9v =
    use Nat +
    a#er8q007v02 + 1
  
  b#p50ctv8q17 : Nat
  b#p50ctv8q17 =
    use Nat +
    a#75jrr0qj1c + 1

```
```unison
a = 777
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    x These definitions would fail on `add` or `update`:
    
      Reason
      conflicted   a   : Nat
    
      Tip: Use `help filestatus` to learn more.

```
```ucm
.nsw> update

  x These definitions failed:
  
    Reason
    conflicted   a   : Nat
  
    Tip: Use `help filestatus` to learn more.

  I tried to auto-apply the patch, but couldn't because it
  contained contradictory entries.

.nsw> view a b

  a#75jrr0qj1c : Nat
  a#75jrr0qj1c = 555
  
  a#er8q007v02 : Nat
  a#er8q007v02 = 444
  
  b#j6aq9c9k9v : Nat
  b#j6aq9c9k9v =
    use Nat +
    a#er8q007v02 + 1
  
  b#p50ctv8q17 : Nat
  b#p50ctv8q17 =
    use Nat +
    a#75jrr0qj1c + 1

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
- [x] check whether creating a name conflict + adding metadata puts the update
      in both categories; if it does, then filter out metadataUpdates from the
      other categories
- [x] add tagging of propagated updates to test propagated updates output
- [x] missing old names in deletion ppe (delete.output.md)  (superseded by \#1143)
- [x] delete.term has some bonkers output
- [x] Make a decision about how we want to show constructors in the diff
- [x] When you delete a name with metadata, it also shows up in updates section
      with the deleted metadata.
- [x] An add with new metadata is getting characterized as an update
- [x] can there be a metadata-only update where it's not a singleton old and new reference
- [x] 12.patch patch needs a space
- [x] This looks like garbage
- [x] Extra 2 blank lines at the end of the add section
- [x] Fix alignment issues with buildTable, convert to column3M (to be written)
- [x] adding an alias is showing up as an Add and a Copy; should just show as Copy
- [x] removing one of multiple aliases appears in removes + moves + copies section
- [x] some overlapping cases between Moves and Copies^
- [x] Maybe don't list the type signature twice for aliases?
