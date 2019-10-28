# Propagating type edits

We introduce a type `Foo` with a function dependent `fooToInt`.

```unison
use .builtin

unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      fooToInt : Foo -> builtin.Int
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
And then we add it.

```ucm
  ☝️  The namespace .subpath is empty.

.subpath> add

  ⍟ I've added these definitions:
  
    unique type Foo
    fooToInt : Foo -> .builtin.Int

.subpath> find.verbose

  1. -- #4d0krm5ahna9i9t4rs8t9mnc62rrh49evl3e85lm0pcqv9plg3u11led2a2she433i2k7ap4ksaqovjn5j9d347hpeh41r250c7d678
     unique type Foo
     
  2. -- #4d0krm5ahna9i9t4rs8t9mnc62rrh49evl3e85lm0pcqv9plg3u11led2a2she433i2k7ap4ksaqovjn5j9d347hpeh41r250c7d678#0
     Foo.Foo : Foo
     
  3. -- #okceomcnpl88n427c1st6sjdjkhr9h5khhqk8je7k83m2khj9h2i6iq109s2nu8726is3l72kv7ontkuo405mtqo1vfl3knpb982hj8
     fooToInt : Foo -> .builtin.Int
     
  

.subpath> view fooToInt

  fooToInt : Foo -> .builtin.Int
  fooToInt _ = +42

```
Then if we change the type `Foo`...

```unison
type Foo = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      type Foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
and update the codebase to use the new type `Foo`...

```ucm
.subpath> update

  ⍟ I've updated to these definitions:
  
    type Foo

  ✅
  
  No conflicts or edits in progress.

```
... it should automatically propagate the type to `fooToInt`.

```ucm
.subpath> view fooToInt

  fooToInt : Foo -> .builtin.Int
  fooToInt _ = +42

```
### Preserving user type variables

We make a term that has a dependency on another term and also a non-redundant
user-provided type signature.

```unison
use .builtin

someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      otherTerm : .builtin.Optional baz -> .builtin.Optional baz
      someTerm  : .builtin.Optional foo -> .builtin.Optional foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Add that to the codebase:

```ucm
  ☝️  The namespace .subpath.preserve is empty.

.subpath.preserve> add

  ⍟ I've added these definitions:
  
    otherTerm : .builtin.Optional baz -> .builtin.Optional baz
    someTerm  : .builtin.Optional foo -> .builtin.Optional foo

```
Let's now edit the dependency:

```unison
use .builtin

someTerm : Optional x -> Optional x
someTerm _ = None
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      someTerm : .builtin.Optional x -> .builtin.Optional x
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Update...

```ucm
.subpath.preserve> update

  ⍟ I've updated to these definitions:
  
    someTerm : .builtin.Optional x -> .builtin.Optional x

  ✅
  
  No conflicts or edits in progress.

```
Now the type of `someTerm` should be `Optional x -> Optional x` and the 
type of `otherTerm` should remain the same.

```ucm
.subpath.preserve> view someTerm

  someTerm : .builtin.Optional x -> .builtin.Optional x
  someTerm _ = .builtin.Optional.None

.subpath.preserve> view otherTerm

  otherTerm : .builtin.Optional baz -> .builtin.Optional baz
  otherTerm y = someTerm y

```
### Propagation only applies to the local branch

```ucm
.> delete.namespace subpath

  🆕
  
  Here's what's changed after the delete:
  
  - Deletes:
  
    subpath.Foo subpath.Foo.Bar subpath.Foo.Foo subpath.fooToInt
    subpath.preserve.otherTerm subpath.preserve.someTerm
  
  Tip: You can always `undo` if this wasn't what you wanted.

```
```unison
use .builtin

someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      otherTerm : builtin.Optional baz -> builtin.Optional baz
      someTerm  : builtin.Optional foo -> builtin.Optional foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
  ☝️  The namespace .subpath.one is empty.

.subpath.one> add

  ⍟ I've added these definitions:
  
    otherTerm : .builtin.Optional baz -> .builtin.Optional baz
    someTerm  : .builtin.Optional foo -> .builtin.Optional foo

.subpath> fork one two

  Done.

```
```unison
use .builtin

someTerm : Optional x -> Optional x
someTerm _ = None
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      someTerm : .builtin.Optional x -> .builtin.Optional x
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.subpath.one> update

  ⍟ I've updated to these definitions:
  
    someTerm : .builtin.Optional x -> .builtin.Optional x

  ✅
  
  No conflicts or edits in progress.

```

```ucm
.subpath.two> view someTerm

  someTerm : .builtin.Optional foo -> .builtin.Optional foo
  someTerm x = x

```
