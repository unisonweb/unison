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
  
    1. foo#jk19sm5bf8 : Nat
       ↓
    2. ┌ foo#0ja1qfpej6 : Nat
    3. └ foo#jk19sm5bf8 : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
A delete should remove both versions of the term.

```ucm
.a> delete foo

  Removed definitions:
  
    1. a.foo#jk19sm5bf8 : Nat
  
  Name changes:
  
    Original               Changes
    2. a.foo#0ja1qfpej6 ┐  3. a.foo#0ja1qfpej6 (removed)
    4. b.foo            ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> ls

  nothing to show

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
  
    1. type Foo#d97e0jhkmd
         
       ↓
    2. ┌ type Foo#d97e0jhkmd
           
    3. └ type Foo#gq9inhvg9h
           
    
    4. Foo.Foo#d97e0jhkmd#0 : Nat -> Foo#d97e0jhkmd
       ↓
    5. ┌ Foo.Foo#d97e0jhkmd#0 : Nat -> Foo#d97e0jhkmd
    6. └ Foo.Foo#gq9inhvg9h#0 : Boolean -> Foo#gq9inhvg9h
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.a> delete Foo

  Removed definitions:
  
    1. type a.Foo#d97e0jhkmd
  
  Name changes:
  
    Original               Changes
    2. a.Foo#gq9inhvg9h ┐  3. a.Foo#gq9inhvg9h (removed)
    4. b.Foo            ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> delete Foo.Foo

  Removed definitions:
  
    1. a.Foo.Foo#d97e0jhkmd#0 : Nat -> #d97e0jhkmd
  
  Name changes:
  
    Original                     Changes
    2. a.Foo.Foo#gq9inhvg9h#0 ┐  3. a.Foo.Foo#gq9inhvg9h#0 (removed)
    4. b.Foo.Foo              ┘  
  
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

  Removed definitions:
  
    1. type foo
    2. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
