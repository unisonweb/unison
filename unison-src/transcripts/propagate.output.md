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
      fooToInt : Foo -> Int

```
And then we add it.

```ucm
  ☝️  The namespace .subpath is empty.

.subpath> add

  ⍟ I've added these definitions:
  
    unique type Foo
    fooToInt : Foo -> Int

.subpath> find.verbose

  1. -- #qae64o6am81hoadf7eabd909gojboi5iu3g9deip79ro18f11bbhir2vg51grg4m72kr5ikdovi6aupttet0nsqil7f0df9nqr10hqg
     unique type Foo
     
  2. -- #qae64o6am81hoadf7eabd909gojboi5iu3g9deip79ro18f11bbhir2vg51grg4m72kr5ikdovi6aupttet0nsqil7f0df9nqr10hqg#0
     Foo.Foo : Foo
     
  3. -- #hvtmbg1bd8of81n2os4ginnnen13njh47294uandlohooq0ej971u6tl5cdsfq237lec1tc007oajc4dee1fmnflqi6ogom3ecemu5g
     fooToInt : Foo -> Int
     
  

.subpath> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42

```
Then if we change the type `Foo`...

```unison
unique type Foo = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo

```
and update the codebase to use the new type `Foo`...

```ucm
.subpath> update

  ⍟ I've updated these names to your new definition:
  
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
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      someTerm : Optional x -> Optional x

```
Update...

```ucm
.subpath.preserve> update

  ⍟ I've updated these names to your new definition:
  
    someTerm : Optional x -> Optional x

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
    2. Foo.Bar            : #16d2id848g
    3. Foo.Foo            : #16d2id848g
    4. fooToInt           : #16d2id848g -> Int
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

```
... in one of the namespaces...

```ucm
.subpath.one> update

  ⍟ I've updated these names to your new definition:
  
    someTerm : Optional x -> Optional x

```
The other namespace should be left alone.

```ucm
.subpath.two> view someTerm

  someTerm : Optional foo -> Optional foo
  someTerm x = x

```
