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

  1. -- #v4a90flt15t54qnjbvbdtj42ouqo8dktu5da8g6q30l4frc6l81ttjtov42r1nbj5jq3hh98snlb64tkbb1mc5dk8les96v71b4qr6g
     unique type Foo
     
  2. -- #v4a90flt15t54qnjbvbdtj42ouqo8dktu5da8g6q30l4frc6l81ttjtov42r1nbj5jq3hh98snlb64tkbb1mc5dk8les96v71b4qr6g#0
     Foo.Foo : Foo
     
  3. -- #5k9rns49vrtujrpbiegajeja9qjjs77fju3usg1i1dpeo44kefkbce776u1kvqhvtutk6a6f178kovr422ocsd4fdsbsg7fprf4o0dg
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
    2. Foo.Bar            : #i2nv821v0u
    3. Foo.Foo            : #i2nv821v0u
    4. fooToInt           : #i2nv821v0u -> Int
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
