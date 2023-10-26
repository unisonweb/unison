Alice tries to merge Bob's branch. Alice has two names for one constructor. This is a merge precondition violation.

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

```
```unison
unique type Foo = MkFoo Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

project/alice> add

  ⍟ I've added these definitions:
  
    unique type Foo

project/alice> alias.term Foo.MkFoo Foo.internal.MkFoo2

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

```
```ucm
project/alice> merge2 bob

  On alice, Foo.MkFoo and Foo.internal.MkFoo2 are aliases. Every
  type declaration must have exactly one name for each
  constructor.

```
