
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
helloWorld = '(printLine "Hello, world!")

type A a = A Nat
ability X a1 a2 where x : Nat
```

```ucm
.ns1> add
.ns1> alias.term fromJust fromJust'
.ns1> alias.term helloWorld helloWorld2
.ns1> names b
.ns1> names bdependent
.ns1> link fromJust b
.ns1> fork .ns1 .ns2
.ns1> cd .
.> diff.namespace ns1 ns2
```

```unison
fromJust = "asldkfjasldkfj"
```

```ucm
.ns1b> add
.> merge ns1b ns1
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
.> diff.namespace ns1 ns2
.> alias.term ns2.d ns2.d'
.> alias.type ns2.A ns2.A'
.> alias.type ns2.X ns2.X'
.> diff.namespace ns1 ns2
.> view 2
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
- [x] An add with new metadata is getting characterized as an update
- [x] can there be a metadata-only update where it's not a singleton old and new reference
- [x] 12.patch patch needs a space
- [x] This looks like garbage
- [x] Extra 2 blank lines at the end of the add section
- [x] Fix alignment issues with buildTable, convert to column3M (to be written)
- [ ] Delete blank line in between copies / renames entries if all entries are 1 to 1
      see todo in the code
- [ ] Things look screwy when the type signature doesn't fit and has to get broken
      up into multiple lines. Maybe just disallow that?
- [ ] add tagging of propagated updates to test propagated updates output
- [x] adding an alias is showing up as an Add and a Copy; should just show as Copy
- [ ] removing one of multiple aliases appears in removes + moves + copies section
  - maybe: anything that's the target of a Move should be filtered out of Removes

      Removes:

        22. ability X2 a1 a2

      Moves:

        23. X2     =>     24. X'

      Copies:

        25. X  ┐   =>     26. X'
        27. X2 ┘

  what about:

    Name changes:

      23. X  ┐  =>  (added)   24. X'
      25. X2 ┘      (removed) 26. X2

      23. X was removed

      23. X  ┐  =>  24. X'
      25. X2 ┘

      24. A     =>   ┌ 25. A'
                     └ 26. A''


  {A,A',B}, {A,A',B}

    No change to {A,A',B}

  {A,A',B}, {A,A'}

    B => {} (B was deleted)

  {A,A',B}, {A,A',B,C}

    {A,A',B} => {C} (C was added as a name)

  Proposal:

    Just have one section for all name changes (removes, moves, copies combined)
    Each item is showing a pair of sets (old names, new names)
    The old names are just shown in their entirety
    The added names are new names - old names
    The removed names are old names - new names
    Added and removed names are shown on the right, tagged as added or removed

      23. X  ┐  =>  (added)   24. X'
      25. X2 ┘      (removed) 26. X2


    Removes:

      11. unique type Y a b
      12. ┌ d  : Nat
      13. └ d'
      12. unique type {Frobnicate,Y} a b
      14. e    : Nat
      15. f    : Nat (+1 metadata)
      16. Y.Y  : a -> b -> Y a b

      -- no special case
      23. X     =>  (removed) 24. X

      -- special case removal (when RHS is empty set)
      23. X     =>  (removed)

      23. X  ┐  =>  (removed)
      25. X2 ┘

      23. X     =>  (removed)
      23. X was removed

      23. X (removed)  ┐
      25. X2 (removed) ┘




  and then "Removes" would mean: the reference no longer exists in the namespace

- [ ] some overlapping cases between Moves and Copies^
- [ ] Make a decision about how we want to show constructors in the diff
- [ ] Maybe don't list the type signature twice for aliases?

      12. ┌ d  : Nat
      13. └ d'
