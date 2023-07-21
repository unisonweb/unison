# reset loose code
```unison
a = 5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    a : Nat

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #nj1rfces5b
  
    + Adds / updates:
    
      a
  
  □ 2. #caij5gocub (start of history)

.> reset 2

  Done.

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #caij5gocub (start of history)

```
```unison
foo.a = 5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo.a : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo.a : Nat

.> ls foo

  1. a (Nat)

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #c0ome1eb6c
  
    + Adds / updates:
    
      foo.a
  
  □ 2. #caij5gocub (start of history)

.> reset 1 foo

  Done.

.> ls foo.foo

  1. a (Nat)

```
# reset branch

```ucm
.> project.create-empty foo

  🎉 I've created the project foo.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

foo/main> history

  ☝️  The namespace  is empty.

```
```unison
a = 5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : Nat

```
```ucm
foo/main> add

  ⍟ I've added these definitions:
  
    a : Nat

foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

foo/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #5l94rduvel (start of history)

```
```unison
a = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      a : Nat

```
```ucm
foo/main> update

  ⍟ I've updated these names to your new definition:
  
    a : Nat

foo/main> reset /topic

  Done.

foo/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #5l94rduvel (start of history)

```
# ambiguous reset

## ambiguous target
```unison
main.a = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main.a : Nat

```
```ucm
foo/main> add

  ⍟ I've added these definitions:
  
    main.a : Nat

foo/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #0i64kpfccl
  
    + Adds / updates:
    
      main.a
  
  □ 2. #5l94rduvel (start of history)

foo/main> reset 2 main

  I'm not sure if you wanted to reset the branch foo/main or the
  namespace main in the current branch. Could you be more
  specific?
  
  1. /main (the branch main in the current project)
  2. main (the relative path main in the current branch)
  
  Tip: use `reset <some hash> 1` or `reset <some hash> 2` to
       pick one of these.

```
## ambiguous hash

```unison
main.a = 3
```

```ucm

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
```ucm
foo/main> switch /topic

foo/topic> add

  ⍟ I've added these definitions:
  
    main.a : Nat

foo/topic> reset main

  I'm not sure if you wanted to reset to the branch foo/main or
  to the namespace main in the current branch. Could you be more
  specific?
  
  1. /main (the branch main in the current project)
  2. main (the relative path main in the current branch)
  
  Tip: use `reset 1` or `reset 2` to pick one of these.

```
