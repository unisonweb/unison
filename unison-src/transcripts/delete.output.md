# Delete

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm
.> delete foo

  ⚠️
  
  I don't know about that name.

```
Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison
foo = 1
structural type Foo = Foo Nat
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    foo : Nat

.> delete foo

  Removed definitions:
  
    1. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete Foo

  Removed definitions:
  
    1. structural type Foo
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete Foo.Foo

  Removed definitions:
  
    1. Foo.Foo : Nat -> #hhc6goudjq
  
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
  
    foo : Nat

```
```unison
foo = 2
```

```ucm
  ☝️  The namespace .b is empty.

.b> add

  ⍟ I've added these definitions:
  
    foo : Nat

.a> merge .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. foo#rrsqv1ogaq : Nat
       ↓
    2. ┌ foo#0t5t522gs3 : Nat
    3. └ foo#rrsqv1ogaq : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
A delete should remove both versions of the term.

```ucm
.a> delete foo

  Removed definitions:
  
    1. a.foo#rrsqv1ogaq : Nat
  
  Name changes:
  
    Original               Changes
    2. b.foo            ┐  3. a.foo#0t5t522gs3 (removed)
    4. a.foo#0t5t522gs3 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> ls

  nothing to show

```
Let's repeat all that on a type, for completeness.

```unison
structural type Foo = Foo Nat
```

```ucm
.a> add

  ⍟ I've added these definitions:
  
    structural type Foo

```
```unison
structural type Foo = Foo Boolean
```

```ucm
.b> add

  ⍟ I've added these definitions:
  
    structural type Foo

.a> merge .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. structural type Foo#hhc6goudjq
         
       ↓
    2. ┌ structural type Foo#gf6ne3ran5
           
    3. └ structural type Foo#hhc6goudjq
           
    
    4. Foo.Foo#hhc6goudjq#0 : Nat -> Foo#hhc6goudjq
       ↓
    5. ┌ Foo.Foo#gf6ne3ran5#0 : Boolean -> Foo#gf6ne3ran5
    6. └ Foo.Foo#hhc6goudjq#0 : Nat -> Foo#hhc6goudjq
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.a> delete Foo

  Removed definitions:
  
    1. structural type a.Foo#hhc6goudjq
  
  Name changes:
  
    Original               Changes
    2. b.Foo            ┐  3. a.Foo#gf6ne3ran5 (removed)
    4. a.Foo#gf6ne3ran5 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> delete Foo.Foo

  Removed definitions:
  
    1. a.Foo.Foo#hhc6goudjq#0 : Nat -> #hhc6goudjq
  
  Name changes:
  
    Original                     Changes
    2. b.Foo.Foo              ┐  3. a.Foo.Foo#gf6ne3ran5#0 (removed)
    4. a.Foo.Foo#gf6ne3ran5#0 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Finally, let's try to delete a term and a type with the same name.

```unison
foo = 1
structural type foo = Foo Nat
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type foo
    foo : Nat

```
```ucm
.> delete foo

  Removed definitions:
  
    1. structural type foo
    2. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
