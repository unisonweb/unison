
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
.ns1> fork .ns1 .ns2
.ns1> cd .
.> diff.namespace ns1 ns2
```

```unison
d = 4
e = 5
```

```ucm
.ns2> add
.> diff.namespace ns1 ns2
.> alias.term ns2.d ns2.d'
.> alias.type ns2.A ns2.A'
.> alias.type ns2.X ns2.X'
.> diff.namespace ns1 ns2
.> view 2
```


## Display issues to fixup

- [x] Extra 2 blank lines at the end of the add section
