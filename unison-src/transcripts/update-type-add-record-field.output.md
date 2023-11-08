```unison
unique type Foo = { bar : Nat }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo

```
```unison
unique type Foo = { bar : Nat, baz : Int }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Foo.baz        : Foo -> Int
      Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
      Foo.baz.set    : Int -> Foo -> Foo
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
```ucm
.> update

  unique type Foo = { bar : Nat, baz : Int }
  
  Foo.bar.modify f = cases Foo bar baz -> Foo (f bar) baz
  
  Foo.bar.set bar1 = cases Foo _ baz -> Foo bar1 baz
  
  Foo.bar = cases Foo bar _ -> bar
  
  Foo.baz.modify f = cases Foo bar baz -> Foo bar (f baz)
  
  Foo.baz.set baz1 = cases Foo bar _ -> Foo bar baz1
  
  Foo.baz = cases Foo _ baz -> baz

  Typechecking failed when propagating the update to all the dependents.

.> view Foo

  unique type Foo = { bar : Nat }

.> find.verbose

  1. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60
     unique type Foo
     
  2. -- #ovhevqfin94qhq5fu0mujfi20mbpvg5mh4vsfklrohp84cch4lhvrn5p29cnbsqfm92l7bt8c1vpjooh72a0psbddvvten4gq2sipag
     Foo.bar : Foo -> Nat
     
  3. -- #as72md2u70e0u9s2ig2ug7jvlbrk1mubo8qlfokpuvgusg35svh05r7nsj27sqo5edeghjnk8g8259fi4ismse736v4n5ojrb3o2le8
     Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
     
  4. -- #5cbctoor75nbtn4ppp10qm1i25gqt2lgth3itqa0lloib32je4ijfj2n3qcdfhmdcnbgum2jg46opntlohv7ladun3dmefl1ucgobeg
     Foo.bar.set : Nat -> Foo -> Foo
     
  5. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60#0
     Foo.Foo : Nat -> Foo
     
  

```
