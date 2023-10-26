Alice tries to merge Bob's branch. In their LCA, `foo` and `bar` are aliases, but in Alice's branch, they are not. This
is a merge precondition violation.

```unison
foo = 10
bar = 10
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : ##Nat
      foo : ##Nat

```
```ucm
.> project.create-empty project

  🎉 I've created the project project.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

project/main> builtins.merge

  Done.

project/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

project/alice> delete.term foo

  Done.

project/alice> delete.term bar

  Done.

```
```unison
foo = 11
bar = 12
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
```ucm
project/alice> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

```
```ucm
project/alice> merge2 bob

  On alice, bar and foo are not aliases, but they used to be.

```
```ucm
project/bob> merge2 alice

  On alice, bar and foo are not aliases, but they used to be.

```
