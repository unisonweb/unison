```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
      makeFoo : Nat -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    makeFoo : Nat -> Foo

```
```unison
structural type Foo = internal.Bar Nat

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

  structural type Foo = internal.Bar Nat

.> find.verbose

  1. -- #68k40ra7l7bmv3m2qebgt14uhqjmiqugadem3eqaqhljgaqnvhmn0urfut51vjml4rdnre9hqkqp9ipn1kr5qs2hocucot2uai7q1mo
     structural type Foo
     
  2. -- #no7n4dnlmm7i8gg0e94fpebd4ciq3o85rcuvtmq7ohh7tvfipivol21u26kuvaq5ct5g60frknt0hucpcdl9c87n61rmjj9ojjvmsko
     Foo.Bar : Nat -> Foo
     
  3. -- #68k40ra7l7bmv3m2qebgt14uhqjmiqugadem3eqaqhljgaqnvhmn0urfut51vjml4rdnre9hqkqp9ipn1kr5qs2hocucot2uai7q1mo#0
     Foo.internal.Bar : Nat -> Foo
     
  4. -- #ptmhmi3lrmec2l22t9el62b6j6qurku52qi9a95dm4c944bemr2m9747bff1qi1pg75h6qr7363af8kdp3iivnd41sd8rbh5n28j6io
     makeFoo : Nat -> Foo
     
  

```
