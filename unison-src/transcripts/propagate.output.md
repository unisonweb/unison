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

  1. -- #ohtefitum4jai7d42hi6vc0vs8kupm9h81t1cspr9600r5grscm7jga452lnns29p7rkm9bq2u4o4ais0praq6r1uarbpfg4mf70758
     unique type Foo
     
  2. -- #ohtefitum4jai7d42hi6vc0vs8kupm9h81t1cspr9600r5grscm7jga452lnns29p7rkm9bq2u4o4ais0praq6r1uarbpfg4mf70758#0
     Foo.Foo : Foo
     
  3. -- #d9h62ubdj054t16h1mv5gc838ji23q8g5kgf35upp8f9ed9rq95pu2jg0lkq4c8k58p2c652o7mc2b7dr21b9m9jng512cop27sdo8o
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
    2. Foo.Bar            : #8ei3pu3p2p
    3. Foo.Foo            : #8ei3pu3p2p
    4. fooToInt           : #8ei3pu3p2p -> Int
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
