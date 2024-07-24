``` unison
unique type Foo = Bar Nat
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Foo

scratch/main> alias.term Foo.Bar Stray.BarAlias

  Done.

```
``` unison
unique type Foo = Bar Nat Nat
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo

```
Bug: we leave `Stray.BarAlias` in the namespace with a nameless decl.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> find.verbose

  1. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g
     type Foo
     
  2. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g#0
     Foo.Bar : Nat -> Nat -> Foo
     
  3. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60#0
     Stray.BarAlias : Nat -> #b509v3eg4k
     
  

```
