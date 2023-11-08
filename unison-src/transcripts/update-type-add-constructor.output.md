```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo
  = Bar Nat
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
  | Baz Nat Nat
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

  structural type Foo = Baz Nat Nat | Bar Nat

.> find.verbose

  1. -- #65mdg7015r81gr9vkvobuf77c1thpoufe8a3d0g0u4k4ck2qmm5pf7thvl5jk02llkcuc9njbbbnhqhru4sd7qsi06ndhr2j5c3k6a0
     structural type Foo
     
  2. -- #65mdg7015r81gr9vkvobuf77c1thpoufe8a3d0g0u4k4ck2qmm5pf7thvl5jk02llkcuc9njbbbnhqhru4sd7qsi06ndhr2j5c3k6a0#1
     Foo.Bar : Nat -> Foo
     
  3. -- #65mdg7015r81gr9vkvobuf77c1thpoufe8a3d0g0u4k4ck2qmm5pf7thvl5jk02llkcuc9njbbbnhqhru4sd7qsi06ndhr2j5c3k6a0#0
     Foo.Baz : Nat -> Nat -> Foo
     
  

```
