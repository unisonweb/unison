# projects merge

```ucm
scratch/main> builtins.merge

  Done.

```
```unison
zonk = 0
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      zonk : Nat

```
```ucm
  ☝️  The namespace .foo is empty.

.foo> add

  ⍟ I've added these definitions:
  
    zonk : ##Nat

scratch/main> project.create-empty foo

  🎉 I've created the project foo.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

scratch/main> merge.old foo foo/main

  ⚠️
  
  The namespace foo doesn't exist.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The namespace foo doesn't exist.

