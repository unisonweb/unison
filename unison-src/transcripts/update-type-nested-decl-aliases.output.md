```unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type A
      structural type A.B
      unique type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type A
    structural type A.B
    unique type Foo

```
```unison
unique type Foo = Bar Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo

```
Bug: this update doesn't do the right thing; we simply don't properly update all of the names because
each decl, in isolation, has two equally good names to pick for its one constructor:

    -- These are the same thing, but which do we render?
    type A = B.OneAlias Foo
    type A = B.TheOtherAlias Foo

    -- Whichever one we picked, we want to pick the other one here
    type A.B = OneAlias Foo
    type A.B = TheOtherAlias Foo

Long story short, we should reject this update as it violates the "decl coherency" precondition.

```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

.> find.verbose

  1. -- #c1loj1j2dpuj2f74v5gdhlnpbmj8np9c5ivmn9od6a0tgr5c9glo647he1b71v99b9l23qqmdod49ad4m2br0ln9vlrf7ld7h79a0r8
     structural type A
     structural type A.B
     
  2. -- #c1loj1j2dpuj2f74v5gdhlnpbmj8np9c5ivmn9od6a0tgr5c9glo647he1b71v99b9l23qqmdod49ad4m2br0ln9vlrf7ld7h79a0r8
     structural type A.B
     structural type A
     
  3. -- #c1loj1j2dpuj2f74v5gdhlnpbmj8np9c5ivmn9od6a0tgr5c9glo647he1b71v99b9l23qqmdod49ad4m2br0ln9vlrf7ld7h79a0r8#0
     A.B.OneAlias, A.OneAlias : Foo -> A
     
  4. -- #tqsrlmuedb3jj0ngl9pbun8411jfu9hst722np0n0b59g19emkbl12mv9avbug10amppj894psav5n36vk0qfdse44jep3vm6h5fh2g#0
     A.B.TheOtherAlias : Foo#b509v3eg4k -> A#tqsrlmuedb
     
  5. -- #c1loj1j2dpuj2f74v5gdhlnpbmj8np9c5ivmn9od6a0tgr5c9glo647he1b71v99b9l23qqmdod49ad4m2br0ln9vlrf7ld7h79a0r8#0
     A.OneAlias, A.B.OneAlias : Foo -> A
     
  6. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g
     unique type Foo
     
  7. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g#0
     Foo.Bar : Nat -> Nat -> Foo
     
  

```
