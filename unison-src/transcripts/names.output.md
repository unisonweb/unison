# `names` command

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      some.otherplace.x : Nat
      some.otherplace.y : Nat
      some.place.x      : Nat
      somewhere.y       : Nat
      somewhere.z       : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    some.otherplace.x : Nat
    some.otherplace.y : Nat
    some.place.x      : Nat
    somewhere.y       : Nat
    somewhere.z       : Nat

```
`names` searches relative to the current path.

```ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.
-- But we don't see somewhere.z which is has the same value but is out of our namespace
.some> names x

  Terms
  Hash:   #gjmq673r1v
  Names:  otherplace.y place.x
  
  Hash:   #pi25gcdv0o
  Names:  otherplace.x
  
  Tip: Use `names.global` to see more results.

-- We can search by hash, and see all aliases of that hash
.some> names #gjmq673r1v

  Term
  Hash:   #gjmq673r1v
  Names:  otherplace.y place.x
  
  Tip: Use `names.global` to see more results.

-- If the query is absolute, treat it as a `names.global`
.some> names .some.place.x

  Term
  Hash:   #gjmq673r1v
  Names:  .some.otherplace.y .some.place.x .somewhere.z
  
  Tip: Use `names.global` to see more results.

```
`names.global` searches from the root, and absolutely qualifies results


```ucm
-- We can search by suffix and find all definitions in the codebase named 'x', and each of their aliases respectively.
.some> names.global x

  Terms
  Hash:   #gjmq673r1v
  Names:  .some.otherplace.y .some.place.x .somewhere.z
  
  Hash:   #pi25gcdv0o
  Names:  .some.otherplace.x

-- We can search by hash, and see all aliases of that hash in the codebase
.some> names.global #gjmq673r1v

  Term
  Hash:   #gjmq673r1v
  Names:  .some.otherplace.y .some.place.x .somewhere.z

-- We can search using an absolute name
.some> names.global .some.place.x

  Term
  Hash:   #gjmq673r1v
  Names:  .some.otherplace.y .some.place.x .somewhere.z

```
