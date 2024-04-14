```ucm
.> project.create-empty update-in

  🎉 I've created the project update-in.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

update-in/main> builtins.merge

  Done.

update-in/main> move.namespace builtin lib.builtin

  Done.

```
```unison
foo.x = "five"
foo.y = x ++ "ty"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo.x : Text
      foo.y : Text

```
```ucm
update-in/main> add

  ⍟ I've added these definitions:
  
    foo.x : Text
    foo.y : Text

```
```unison
x = "six"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Text

```
```ucm
update-in/main> update-in foo

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

```
```unison
> y
```

```ucm

  Loading changes detected in scratch.u.

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > y
          ⧩
          "sixty"

```
