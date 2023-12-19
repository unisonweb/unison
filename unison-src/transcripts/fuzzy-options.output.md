# Test that the options selector for fuzzy finding is working as expected for different argument types.


```unison
optionOne = 1

nested.optionTwo = 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested.optionTwo : ##Nat
      optionOne        : ##Nat

```
Definition args

```ucm
.> add

  ⍟ I've added these definitions:
  
    nested.optionTwo : ##Nat
    optionOne        : ##Nat

.> debug.fuzzy-options view _

  Select a definition:
    * optionOne
    * nested.optionTwo

```
Namespace args

```ucm
.> add

  ⊡ Ignored previously added definitions: nested.optionTwo
    optionOne

.> debug.fuzzy-options cd _

  Select a namespace:
    * nested

```
Project Branch args

```ucm
.> project.create-empty myproject

  🎉 I've created the project myproject.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

myproject/main> branch mybranch

  Done. I've created the mybranch branch based off of main.
  
  Tip: Use `merge /mybranch /main` to merge your work back into
       the main branch.

.> debug.fuzzy-options switch _

  Select a project or branch:
    * myproject/main
    * myproject/mybranch
    * myproject

```
