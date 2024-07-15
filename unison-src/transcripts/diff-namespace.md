```ucm:hide
scratch/b1> builtins.merge lib.builtins
scratch/b2> builtins.merge lib.builtins
scratch/nsx> builtins.merge lib.builtins
scratch/main> builtins.merge lib.builtins
scratch/ns1> builtins.merge lib.builtins
```

```unison:hide
x = 23
fslkdjflskdjflksjdf = 663
```

```ucm
scratch/b1> add
```

```unison:hide
x = 23
fslkdjflskdjflksjdf = 23
abc = 23
```

```ucm
scratch/b2> add
scratch/b1> debug.alias.term.force .x .fslkdjflskdjflksjdf
```

```ucm
scratch/main> diff.namespace /b1: /b2:
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

```unison:hide
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
scratch/ns1> alias.term fromJust fromJust'
scratch/ns1> alias.term helloWorld helloWorld2
scratch/ns1> branch /ns2
```

Here's what we've done so far:

```ucm:error
scratch/main> diff.namespace .nothing /ns1:
```

```ucm:error
scratch/main> diff.namespace /ns1: /ns2:
```

```unison:hide
junk = "asldkfjasldkfj"
```

```ucm
scratch/ns1> add
scratch/ns1> debug.alias.term.force junk fromJust
scratch/ns1> delete.term junk
```

```unison:hide
fromJust = 99
b = 999999999
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

```ucm
scratch/ns2> update
scratch/main> diff.namespace /ns1: /ns2:
scratch/ns2> alias.term d d'
scratch/ns2> alias.type A A'
scratch/ns2> alias.type X X'
scratch/main> diff.namespace /ns1: /ns2:
scratch/ns1> alias.type X X2
scratch/ns2> alias.type A' A''
scratch/ns2> branch /ns3
scratch/ns2> alias.term fromJust' yoohoo
scratch/ns2> delete.term.verbose fromJust'
scratch/main> diff.namespace /ns3: /ns2:
```
```unison:hide
bdependent = "banana"
```
```ucm
scratch/ns3> update
scratch/main> diff.namespace /ns2: /ns3:
```


## Two different auto-propagated changes creating a name conflict

Currently, the auto-propagated name-conflicted definitions are not explicitly
shown, only their also-conflicted dependency is shown.

```unison:hide
a = 333
b = a + 1

forconflicts = 777
```

```ucm
scratch/nsx> add
scratch/nsx> branch /nsy
scratch/nsx> branch /nsz
```

```unison:hide
a = 444
```

```ucm
scratch/nsy> update
```

```unison:hide
a = 555
```

```ucm
scratch/nsz> update
scratch/nsy> branch /nsw
scratch/nsw> debug.alias.term.force .forconflicts .a
scratch/nsw> debug.alias.term.force .forconflicts .b
```

```ucm
scratch/main> diff.namespace /nsx: /nsw:
scratch/nsw> view a
scratch/nsw> view b
```

## Should be able to diff a namespace hash from history.

```unison
x = 1
```

```ucm
scratch/hashdiff> add
```

```unison
y = 2
```

```ucm
scratch/hashdiff> add
scratch/hashdiff> history
scratch/hashdiff> diff.namespace 2 1
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
