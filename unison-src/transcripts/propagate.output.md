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

  1. -- #rh8gqhulifnkfja1sud8mr4djlcnpg4sqhbqd75ij6j4eibamfcf2hf7cuhqqusqttvbmhj41khd8p8m1jpjdcdla1ul8ved9jk94po
     unique type Foo
     
  2. -- #rh8gqhulifnkfja1sud8mr4djlcnpg4sqhbqd75ij6j4eibamfcf2hf7cuhqqusqttvbmhj41khd8p8m1jpjdcdla1ul8ved9jk94po#0
     Foo.Foo : Foo
     
  3. -- #fg2agohofnn72hd471knaui211ktq0p35oq9gi3f1gjcl2kog1vtqdkd8qmod562vbtrjh2nvnejj028blnmbj3ajh3askfo0uj4sbg
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
