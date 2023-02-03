# Delete

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm
.> delete.verbose foo

  ⚠️
  
  I don't know about those names.

```
Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison
foo = 1
structural type Foo = Foo ()
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    foo : Nat

.> delete.verbose foo

  Removed definitions:
  
    1. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete.verbose Foo

  Removed definitions:
  
    1. structural type Foo
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete.verbose Foo.Foo

  Removed definitions:
  
    1. Foo.Foo : '#089vmor9c5
  
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
  
    1. foo#gjmq673r1v : Nat
       ↓
    2. ┌ foo#dcgdua2lj6 : Nat
    3. └ foo#gjmq673r1v : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
A delete should remove both versions of the term.

```ucm
.a> delete.verbose foo

  Removed definitions:
  
    1. a.foo#gjmq673r1v : Nat
  
  Name changes:
  
    Original               Changes
    2. b.foo            ┐  3. a.foo#dcgdua2lj6 (removed)
    4. a.foo#dcgdua2lj6 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> ls

  nothing to show

```
Let's repeat all that on a type, for completeness.

```unison
structural type Foo = Foo ()
```

```ucm
.a> add

  ⍟ I've added these definitions:
  
    structural type Foo

```
```unison
structural type Foo = Foo
```

```ucm
.b> add

  ⍟ I've added these definitions:
  
    structural type Foo

.a> merge .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. structural type Foo#089vmor9c5
         
       ↓
    2. ┌ structural type Foo#00nv2kob8f
           
    3. └ structural type Foo#089vmor9c5
           
    
    4. Foo.Foo#089vmor9c5#0 : 'Foo#089vmor9c5
       ↓
    5. ┌ Foo.Foo#00nv2kob8f#0 : ()
    6. └ Foo.Foo#089vmor9c5#0 : 'Foo#089vmor9c5
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```ucm
.a> delete.verbose Foo

  Removed definitions:
  
    1. structural type a.Foo#089vmor9c5
  
  Name changes:
  
    Original               Changes
    2. b.Foo            ┐  3. a.Foo#00nv2kob8f (removed)
    4. builtin.Unit     │  
    5. a.Foo#00nv2kob8f ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
```ucm
.a> delete.verbose Foo.Foo

  Removed definitions:
  
    1. a.Foo.Foo#089vmor9c5#0 : '#089vmor9c5
  
  Name changes:
  
    Original                     Changes
    2. b.Foo.Foo              ┐  3. a.Foo.Foo#00nv2kob8f#0 (removed)
    4. builtin.Unit.Unit      │  
    5. a.Foo.Foo#00nv2kob8f#0 ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Finally, let's try to delete a term and a type with the same name.

```unison
foo = 1
structural type foo = Foo ()
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type foo
    foo : Nat

```
```ucm
.> delete.verbose foo

  Removed definitions:
  
    1. structural type foo
    2. foo : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
We want to be able to delete multiple terms at once

```unison
a = "a"
b = "b"
c = "c"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    a : Text
    b : Text
    c : Text

.> delete.verbose a b c

  Removed definitions:
  
    1. a : Text
    2. b : Text
    3. c : Text
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
We can delete terms and types in the same invocation of delete

```unison
structural type Foo = Foo ()
a = "a"
b = "b"
c = "c"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    a : Text
    b : Text
    c : Text

.> delete.verbose a b c Foo

  Removed definitions:
  
    1. structural type Foo
    2. a : Text
    3. b : Text
    4. c : Text
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> delete.verbose Foo.Foo

  Name changes:
  
    Original      Changes
    1. Foo.Foo ┐  2. Foo.Foo (removed)
    3. foo.Foo ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
We can delete a type and its constructors

```unison
structural type Foo = Foo ()
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo

.> delete.verbose Foo Foo.Foo

  Removed definitions:
  
    1. structural type Foo
  
  Name changes:
  
    Original      Changes
    2. Foo.Foo ┐  3. Foo.Foo (removed)
    4. foo.Foo ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
You should not be able to delete terms which are referenced by other terms

```unison
a = 1
b = 2
c = 3
d = a + b + c
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat
      (also named b.foo)
    c : Nat
    d : Nat

.> delete.verbose a b c

  ⚠️
  
  I didn't delete the following definitions because they are
  still in use:
  
  Dependency   Referenced In
  c            1. d
               
  a            2. d

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  I didn't delete the following definitions because they are
  still in use:
  
  Dependency   Referenced In
  c            1. d
               
  a            2. d

