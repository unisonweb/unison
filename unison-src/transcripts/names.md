# `names` command

```ucm:hide
.> builtins.mergeio
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
.> add
```


`names` searches relative to the current path.

```ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.
-- But we don't see somewhere.z which is has the same value but is out of our namespace
.some> names x
-- We can search by hash, and see all aliases of that hash
.some> names #gjmq673r1v
-- If the query is absolute, treat it as a `names.global`
.some> names .some.place.x
```

`names.global` searches from the root, and absolutely qualifies results


```ucm
-- We can search by suffix and find all definitions in the codebase named 'x', and each of their aliases respectively.
.some> names.global x
-- We can search by hash, and see all aliases of that hash in the codebase
.some> names.global #gjmq673r1v
-- We can search using an absolute name
.some> names.global .some.place.x
```
