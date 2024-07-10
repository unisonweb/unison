# `names` command

```ucm
scratch/main> builtins.merge lib.builtins
```

Example uses of the `names` command and output

```unison
-- Some names with the same value
some.place.x = 1
some.otherplace.y = 1
some.otherplace.x = 10
somewhere.z = 1
-- Some similar name with a different value
somewhere.y = 2
```

```ucm
scratch/main> add
```


`names` searches relative to the current path.

```ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.
scratch/main> names x
-- We can search by hash, and see all aliases of that hash
scratch/main> names #gjmq673r1v
-- Works with absolute names too
scratch/main> names .some.place.x
```

`names.global` searches from the root, and absolutely qualifies results


TODO: swap this back to a 'ucm' block when names.global is re-implemented

```
-- We can search from a different branch and find all names in the codebase named 'x', and each of their aliases respectively.
scratch/other> names.global x
-- We can search by hash, and see all aliases of that hash in the codebase
scratch/other> names.global #gjmq673r1v
-- We can search using an absolute name
scratch/other> names.global .some.place.x
```
