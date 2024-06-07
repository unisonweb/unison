```ucm
.> project.create-empty test-ls

  🎉 I've created the project test-ls.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

test-ls/main> builtins.merge

  Done.

```
```unison
foo.bar.add x y = x Int.+ y

foo.bar.subtract x y = x Int.- y
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo.bar.add      : Int -> Int -> Int
      foo.bar.subtract : Int -> Int -> Int

```
```ucm
test-ls/main> add

  ⍟ I've added these definitions:
  
    foo.bar.add      : Int -> Int -> Int
    foo.bar.subtract : Int -> Int -> Int

test-ls/main> ls foo

  1. bar/ (2 terms)

test-ls/main> ls 1

```



🛑

The transcript failed due to an error in the stanza above. The error is:

Expected a namespace, but the numbered arg resulted in foo.bar, which is a name.
