# Delete

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm
scratch/main> delete.verbose foo

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    foo

```
Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison
foo = 1
structural type Foo = Foo ()
```

```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    structural type Foo
    foo : Nat

scratch/main> delete.verbose foo

  Removed definitions:
  
    1. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

scratch/main> delete.verbose Foo

  Removed definitions:
  
    1. structural type Foo
  
  Tip: You can use `undo` or `reflog` to undo this change.

scratch/main> delete.verbose Foo.Foo

  Removed definitions:
  
    1. Foo.Foo : '#089vmor9c5
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
How about an ambiguous term?

```unison
foo = 1
```

```ucm
  ☝️  The namespace .a is empty.

.a> add

  ⍟ I've added these definitions:
  
    foo : ##Nat

```
```unison
foo = 2
```

```ucm
  ☝️  The namespace .b is empty.

.b> add

  ⍟ I've added these definitions:
  
    foo : ##Nat

.a> merge.old .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. foo#gjmq673r1v : ##Nat
       ↓
    2. ┌ foo#dcgdua2lj6 : ##Nat
    3. └ foo#gjmq673r1v : ##Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

```
A delete should remove both versions of the term.

```ucm
scratch/main> delete.verbose a.foo

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    a.foo

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    a.foo

