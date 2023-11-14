```unison
unique type Foo = Bar Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo

.> alias.term Foo.Bar Foo.BarAlias

  Done.

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
Bug: we leave `Foo.BarAlias` in the namespace with a nameless decl.

```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

.> find.verbose

  1. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g
     unique type Foo
     
  2. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g#0
     Foo.Bar : Nat -> Nat -> Foo
     
  3. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60#0
     Foo.BarAlias : Nat -> Foo#b509v3eg4k
     
  

```
