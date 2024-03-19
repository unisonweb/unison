# tests for the new merge command

## Basic fast-forward merge

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

proj/main> merge2 /topic

  I merged topic into main.

proj/main> view bar

  bar : Nat
  bar =
    use Nat +
    foo + 1

```
## Add/Add agree

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Nat
foo = 1

bar : Nat
bar = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

proj/main> merge2 /topic

  I merged topic into main.

```
## Add/Add conflict

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Nat
foo = 4
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 4

foo : Nat
foo = 1
```

## Update/Update conflict

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
foo : Nat
foo = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```unison
foo : Nat
foo = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```ucm
proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 3

foo : Nat
foo = 2
```

## Update/Update agree

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
foo : Nat
foo = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```unison
foo : Nat
foo = 2

bar : Nat
bar = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

```
## Update/Delete conflict

We don't consider these, so this transcript is capturing our
ignorance.

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

```
```unison
foo : Nat
foo = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
We silently ignore the delete

```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> view foo

  foo : Nat
  foo = 2

```
## Alice deletes x bob adds y

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/main> delete.term foo

  Done.

```
```unison
bar : ()
bar = ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : ()

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : ()

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> ls

  1. bar      (())
  2. builtin/ (627 terms, 89 types)

```
## Alice adds x bob deletes y

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

```
```unison
bar : ()
bar = ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : ()

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    bar : ()

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> ls

  1. bar      (())
  2. builtin/ (627 terms, 89 types)

```
## Alice deletes x bob deletes x

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

proj/main> delete.term foo

  Done.

```
```ucm
proj/main> merge2 /topic

  😶
  
  proj/main was already up-to-date with proj/topic.

proj/main> ls

  1. builtin/ (627 terms, 89 types)

```
## Altered dependent

`foo : Nat` is in the ancestor of `main` and `topic`

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
`topic` adds a dependent of `foo`

```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

```
`main` changes the type of `foo`

```unison
foo : Int
foo = +1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
attempt to merge `topic` into `main`

```ucm
proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Nat
bar =
  use Nat +
  foo + 1
```

## Precondition violations

### term in lib


```unison
lib.foo : Nat
lib.foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lib.foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    lib.foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
bonk : Nat
bonk = 5
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bonk : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bonk : Nat

```
```ucm
proj/main> merge2 /topic

  Defns in lib

```
### Constructor alias

```unison
unique type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> alias.term Foo.Bar Foo.Alias

  Done.

```
```ucm
proj/main> merge2 /topic

  On topic, Foo.Alias and Foo.Bar are aliases. Every type
  declaration must have exactly one name for each constructor.

```
### Missing constructor

```unison
unique type Foo = Bar | Baz
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term Foo.Bar

  Done.

```
```ucm
proj/main> merge2 /topic

  Missing constructor name.

```
### Nested decl alias

```unison
structural type Foo = FooCon
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
        (also named builtin.Unit)

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    structural type Foo
      (also named builtin.Unit)

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
structural type Foo.Bar = BarCon
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo.Bar
        (also named Foo and builtin.Unit)

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    structural type Foo.Bar
      (also named Foo and builtin.Unit)

```
```ucm
proj/main> merge2 /topic

  Nested decl alias.

```
### Stray constructor alias

```unison
unique type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> alias.term Foo.Bar Stray

  Done.

```
```ucm
proj/main> merge2 /topic

  Stray constructor.

```
