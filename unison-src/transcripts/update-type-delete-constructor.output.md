```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo
  = Bar Nat
  | Baz Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo

```
```unison
structural type Foo
  = Bar Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Foo

```
```ucm
.> update

  I propagated the update and am now saving the results.

  Done.

.> view Foo

  structural type Foo = Bar Nat

.> find.verbose

  1. -- #68k40ra7l7bmv3m2qebgt14uhqjmiqugadem3eqaqhljgaqnvhmn0urfut51vjml4rdnre9hqkqp9ipn1kr5qs2hocucot2uai7q1mo
     structural type Foo
     
  2. -- #68k40ra7l7bmv3m2qebgt14uhqjmiqugadem3eqaqhljgaqnvhmn0urfut51vjml4rdnre9hqkqp9ipn1kr5qs2hocucot2uai7q1mo#0
     Foo.Bar : Nat -> Foo
     
  

```
