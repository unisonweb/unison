```unison
a.b.one = 1
a.two = 2

a.x.three = 3
a.x.four = 4

structural type a.x.Foo = Foo | Bar
structural type a.b.Baz = Boo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type a.b.Baz
      structural type a.x.Foo
      a.b.one   : ##Nat
      a.two     : ##Nat
      a.x.four  : ##Nat
      a.x.three : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type a.b.Baz
    structural type a.x.Foo
    a.b.one   : ##Nat
    a.two     : ##Nat
    a.x.four  : ##Nat
    a.x.three : ##Nat

.> delete.term a.b.one

  Removed definitions:
  
    1. a.b.one : ##Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> alias.term a.two a.newtwo

  Done.

.> move.namespace a.x a.y

  Done.

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #tteooc9j2d
  
    > Moves:
    
      Original name New name
      a.x.Foo       a.y.Foo
      a.x.Foo.Bar   a.y.Foo.Bar
      a.x.Foo.Foo   a.y.Foo.Foo
      a.x.four      a.y.four
      a.x.three     a.y.three
  
  ⊙ 2. #bicrtgqj12
  
    + Adds / updates:
    
      a.newtwo
    
    = Copies:
    
      Original name New name(s)
      a.two         a.newtwo
  
  ⊙ 3. #bofp4huk1j
  
    - Deletes:
    
      a.b.one
  
  □ 4. #gss5s88mo3 (start of history)

.> debug.name-diff 4 1

  Kind   Name          Ref                                                                                                           Change
  Term   a.newtwo      #dcgdua2lj6upd1ah5v0qp09gjsej0d77d87fu6qn8e2qrssnlnmuinoio46hiu53magr7qn8vnqke8ndt0v76700o5u8gcvo7st28jg      Added
  Term   a.y.four      #vcfbbslncd2qloc03kalgsmufl3j5es6cehcrbmlj6t78d4uk5j9gpa3hhf2opln1u2kiepg5n2cn49ianf2oig0mi4c2ldn1r9lf40      Added
  Term   a.y.three     #f3lgjvjqoocpt8v6kdgd2bgthh11a7md3qdp9rf5datccmo580btjd5bt5dro3irqs0is7vm7s1dphddjbtufch620te7ef7canmjj8      Added
  Term   a.y.Foo.Bar   #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0#d1   Added
  Term   a.y.Foo.Foo   #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0#d0   Added
  Term   a.b.one       #gjmq673r1vrurfotlnirv7vutdhm6sa3s02em5g22kk606mv6duvv8be402dv79312i4a0onepq5bo7citsodvq2g720nttj0ee9p0g      Removed
  Term   a.x.four      #vcfbbslncd2qloc03kalgsmufl3j5es6cehcrbmlj6t78d4uk5j9gpa3hhf2opln1u2kiepg5n2cn49ianf2oig0mi4c2ldn1r9lf40      Removed
  Term   a.x.three     #f3lgjvjqoocpt8v6kdgd2bgthh11a7md3qdp9rf5datccmo580btjd5bt5dro3irqs0is7vm7s1dphddjbtufch620te7ef7canmjj8      Removed
  Term   a.x.Foo.Bar   #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0#d1   Removed
  Term   a.x.Foo.Foo   #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0#d0   Removed
  Type   a.y.Foo       #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0      Added
  Type   a.x.Foo       #6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0      Removed

```
