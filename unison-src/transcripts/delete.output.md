# Delete

The delete command can delete both terms and types, as long as it's given an
unambiguous name.

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
type Foo = Foo Nat
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    type Foo
    foo : Nat

.> delete foo

  Removed definitions:
  
    1. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete Foo

  Removed definitions:
  
    1. type Foo
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete Foo.Foo

  Removed definitions:
  
    1. Foo.Foo : Nat -> #d97e0jhkmd
  
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
  
    1. foo#jk19 : Nat
       ↓
    2. ┌ foo#0ja1 : Nat
    3. └ foo#jk19 : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.a> delete foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    foo#0ja1
    foo#jk19
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
I can force my delete through by re-issuing the command.

```ucm
.a> delete foo

  Removed definitions:
  
    1. a.foo#jk19 : Nat
  
  Name changes:
  
    Original         Changes
    2. b.foo      ┐  3. a.foo#0ja1 (removed)
    4. a.foo#0ja1 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Let's repeat all that on a type, for completeness.

```unison
type Foo = Foo Nat
```

```ucm
.a> add

  ⍟ I've added these definitions:
  
    type Foo

```
```unison
type Foo = Foo Boolean
```

```ucm
.b> add

  ⍟ I've added these definitions:
  
    type Foo

.a> merge .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. type Foo#d97e
         
       ↓
    2. ┌ type Foo#d97e
           
    3. └ type Foo#gq9i
           
    
    4. Foo.Foo#d97e#0 : Nat -> Foo
       ↓
    5. ┌ Foo.Foo#d97e#0 : Nat -> Foo
    6. └ Foo.Foo#gq9i#0 : Boolean -> b.Foo
  
  Added definitions:
  
    7. foo : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.a> delete Foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    Foo#d97e
    Foo#gq9i
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.a> delete Foo

  Removed definitions:
  
    1. type a.Foo#d97e
  
  Name changes:
  
    Original         Changes
    2. b.Foo      ┐  3. a.Foo#gq9i (removed)
    4. a.Foo#gq9i ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> delete Foo.Foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    Foo.Foo#d97e#0
    Foo.Foo#gq9i#0
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.a> delete Foo.Foo

  Removed definitions:
  
    1. a.Foo.Foo#d97e#0 : Nat -> #d97e0jhkmd
  
  Name changes:
  
    Original               Changes
    2. b.Foo.Foo        ┐  3. a.Foo.Foo#gq9i#0 (removed)
    4. a.Foo.Foo#gq9i#0 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Finally, let's try to delete a term and a type with the same name.

```unison
foo = 1
type foo = Foo Nat
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    type foo
    foo : Nat

```
```ucm
.> delete foo

  🤔
  
  That name is ambiguous. It could refer to any of the following
  definitions:
  
    foo#jk19
    foo#d97e
  
  You may:
  
    * Delete one by an unambiguous name, given above.
    * Delete them all by re-issuing the previous command.

```
```ucm
.> delete foo

  Removed definitions:
  
    1. type foo
    2. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
