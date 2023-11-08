```unison
unique type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      makeFoo : Nat -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo
    makeFoo : Nat -> Foo

```
```unison
unique type Foo = internal.Bar Nat

Foo.Bar : Nat -> Foo
Foo.Bar n = internal.Bar n
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⊡ Previously added definitions will be ignored: Foo
    
    ⍟ These new definitions are ok to `add`:
    
      Foo.Bar : Nat -> Foo

```
```ucm
.> update

  I propagated the update and am now saving the results.

  Done.

.> view Foo

  unique type Foo = internal.Bar Nat

.> find.verbose

  1. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60
     unique type Foo
     
  2. -- #qp7bkhdbv3b8fphm9lhr5nnuu1d2hb0aesr3hc5i212krdgpa953gqmlac9ehfisjp38jlvrftnuehpcveampsgl1ouogki4rnqdbn8
     Foo.Bar : Nat -> Foo
     
  3. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60#0
     Foo.internal.Bar : Nat -> Foo
     
  4. -- #02rqdk3rbj79g7dnvbaa6sc5gcc6q24sa4smpok88nkhqc0atbpvhklpuain8bn7s0nt0ivsi0ln69oqvsasagvqlh700928cbgdki0
     makeFoo : Nat -> Foo
     
  

```
