# Propagating type edits

We introduce a type `Foo` with a function dependent `fooToInt`.

```unison
use .builtin

unique [foo1] type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      fooToInt : Foo -> Int
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
And then we add it.

```ucm
  ☝️  The namespace .subpath is empty.

.subpath> add

  ⍟ I've added these definitions:
  
    unique type Foo
    fooToInt : Foo -> Int

.subpath> find.verbose

  1. -- #6ccohugs0p0rkd8cgfecjgot1djob2v486rfkf6g2o1lr34nsk6r3dgtjv096sokend68h5ae7vojsvaajkulr4pipe4bjmu6du4mpo
     unique type Foo
     
  2. -- #6ccohugs0p0rkd8cgfecjgot1djob2v486rfkf6g2o1lr34nsk6r3dgtjv096sokend68h5ae7vojsvaajkulr4pipe4bjmu6du4mpo#0
     Foo.Foo : Foo
     
  3. -- #o9q6anf4873hbnsaiifh5b46q8fdli18cu8cudu0ti8ort1gm253120uq8ijk24l52ecf62bm1rmq4tgnu7ip8apireh1oq97e042jg
     fooToInt : Foo -> Int
     
  

.subpath> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42

```
Then if we change the type `Foo`...

```unison
unique [foo2] type Foo = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      unique type Foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
and update the codebase to use the new type `Foo`...

```ucm
.subpath> update

  ⍟ I've updated to these definitions:
  
    unique type Foo

```
... it should automatically propagate the type to `fooToInt`.

```ucm
.subpath> view fooToInt

  fooToInt : Foo -> Int
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
    
      otherTerm : Optional baz -> Optional baz
      someTerm  : Optional foo -> Optional foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Add that to the codebase:

```ucm
  ☝️  The namespace .subpath.preserve is empty.

.subpath.preserve> add

  ⍟ I've added these definitions:
  
    otherTerm : Optional baz -> Optional baz
    someTerm  : Optional foo -> Optional foo

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
    
      someTerm : Optional x -> Optional x
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Update...

```ucm
.subpath.preserve> update

  ⍟ I've updated to these definitions:
  
    someTerm : .builtin.Optional x -> .builtin.Optional x

```
Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

```ucm
.subpath.preserve> view someTerm

  someTerm : Optional x -> Optional x
  someTerm _ = None

.subpath.preserve> view otherTerm

  otherTerm : Optional baz -> Optional baz
  otherTerm y = someTerm y

```
### Propagation only applies to the local branch

Cleaning up a bit...

```ucm
.> delete.namespace subpath

  Removed definitions:
  
    1. unique type Foo
    2. Foo.Bar            : #o4dd8v5jef
    3. Foo.Foo            : #o4dd8v5jef
    4. fooToInt           : #o4dd8v5jef -> Int
    5. preserve.otherTerm : Optional baz -> Optional baz
    6. preserve.someTerm  : Optional x -> Optional x
    7. patch patch
    8. patch preserve.patch
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Now, we make two terms, where one depends on the other.

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
    
      otherTerm : Optional baz -> Optional baz
      someTerm  : Optional foo -> Optional foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
We'll make two copies of this namespace.

```ucm
  ☝️  The namespace .subpath.one is empty.

.subpath.one> add

  ⍟ I've added these definitions:
  
    otherTerm : Optional baz -> Optional baz
    someTerm  : Optional foo -> Optional foo

.subpath> fork one two

  Done.

```
Now let's edit one of the terms...

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
    
      someTerm : Optional x -> Optional x
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
... in one of the namespaces...

```ucm
.subpath.one> update

  ⍟ I've updated to these definitions:
  
    someTerm : .builtin.Optional x -> .builtin.Optional x

```
The other namespace should be left alone.

```ucm
.subpath.two> view someTerm

  someTerm : Optional foo -> Optional foo
  someTerm x = x

```
