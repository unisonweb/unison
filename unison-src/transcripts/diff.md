
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
a = 1
b = 2
c = 3

type A a = A Nat
ability X a1 a2 where x : Nat
```

```ucm
.ns1> add
.ns1> link a b
.ns1> fork .ns1 .ns2
.ns1> cd .
.> diff.namespace ns1 ns2
```

```unison
a = 99
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
.> view 2
.> link ns2.f ns1.a
.> diff.namespace ns1 ns2
.> unlink ns2.a ns2.b
.> diff.namespace ns1 ns2
```





## Display issues to fixup

- [x] An add with new metadata is getting characterized as an update
- [ ] 12.patch patch needs a space
- [ ] This looks like garbage
  Updates:

    1. [f : Nat

       ⧩ replaced with
    2. [f : Nat
            + 3.a  : Nat


    Things busted:
      [x] Get rid of blank line before replaced with
      [x] Missing a space after the metadata 3. number
      3. Square brackets look bad, that was a bad idea and we
         should feel bad.
      [x] Metadata is indented waaay too far. Just be 1 level.

- [ ] Try deleting blank line in between copies / renames entries
- [x] Extra 2 blank lines at the end of the add section
