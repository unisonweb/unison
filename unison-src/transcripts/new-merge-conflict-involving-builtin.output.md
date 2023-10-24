```ucm
.> builtins.merge

  Done.

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

```
Make an add-add conflict involving a builtin and observe that violates a merge precondition.

```unison
structural type Foo = MkFoo Nat Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo

```
```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

project/alice> alias.type builtin.Nat Foo

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

project/bob> add

  ⍟ I've added these definitions:
  
    structural type Foo

```
```ucm
project/alice> merge2 bob

  Conflict involving builtin.

```
```ucm
project/bob> merge2 alice

  Conflict involving builtin.

```
