
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

```unison:hide
fromJust = 1
b = 2
bdependent = b
c = 3
helloWorld = '(printLine "Hello, world!")

type A a = A Nat
ability X a1 a2 where x : Nat
```

```ucm
.ns1> add
.ns1> alias.term fromJust fromJust'
.ns1> alias.term helloWorld helloWorld2
.ns1> link fromJust b
.ns1> fork .ns1 .ns2
.ns1> cd .
.> diff.namespace ns1 ns2
```

```unison:hide
fromJust = "asldkfjasldkfj"
```

```ucm
.ns1b> add
.> merge ns1b ns1
```

```unison:hide
fromJust = 99
b = "oog"
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

```ucm
.ns2> update
.> diff.namespace ns1 ns2
.> alias.term ns2.d ns2.d'
.> alias.type ns2.A ns2.A'
.> alias.type ns2.X ns2.X'
.> diff.namespace ns1 ns2
.> link ns2.f ns1.c
.> diff.namespace ns1 ns2
.> unlink ns2.fromJust ns2.b
.> diff.namespace ns1 ns2
.> alias.type ns1.X ns1.X2
.> alias.type ns2.A' ns2.A''
.> delete.term ns2.fromJust'
.> diff.namespace ns1 ns2
.> diff.namespace ns2 nsempty
```

## Display issues to fixup
- [ ] Things look screwy when the type signature doesn't fit and has to get broken
      up into multiple lines. Maybe just disallow that?
- [ ] add tagging of propagated updates to test propagated updates output
- [ ] Make a decision about how we want to show constructors in the diff
- [ ] Delete blank line in between copies / renames entries if all entries are 1 to 1
      see todo in the code
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
