# `names` command

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.

```
Example uses of the `names` command and output

``` unison
-- Some names with the same value
some.place.x = 1
some.otherplace.y = 1
some.otherplace.x = 10
somewhere.z = 1
-- Some similar name with a different value
somewhere.y = 2
```

``` ucm

  Loading changes detected in scratch.u.

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
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    some.otherplace.x : Nat
    some.otherplace.y : Nat
    some.place.x      : Nat
    somewhere.y       : Nat
    somewhere.z       : Nat

```
`names` searches relative to the current path.

``` ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.
scratch/main> names x

  Terms
  Hash:   #gjmq673r1v
  Names:  some.otherplace.y some.place.x somewhere.z
  
  Hash:   #pi25gcdv0o
  Names:  some.otherplace.x
  
  Tip: Use `names.global` to see more results.

-- We can search by hash, and see all aliases of that hash
scratch/main> names #gjmq673r1v

  Term
  Hash:   #gjmq673r1v
  Names:  some.otherplace.y some.place.x somewhere.z
  
  Tip: Use `names.global` to see more results.

-- Works with absolute names too
scratch/main> names .some.place.x

  Term
  Hash:   #gjmq673r1v
  Names:  some.otherplace.y some.place.x somewhere.z
  
  Tip: Use `names.global` to see more results.

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

