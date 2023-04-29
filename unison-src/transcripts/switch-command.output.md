The `switch` command switches to an existing project or branch.

Setup stuff.

```unison
someterm = 18
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      someterm : Nat

```
```ucm
.> project.create foo

  I just created project foo with branch main.

foo/main> add

  ⍟ I've added these definitions:
  
    someterm : Nat

foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
Now, the demo.

```ucm
.> switch foo

.> switch foo/topic

```
```ucm
.> switch foo/no-such-branch

  foo/no-such-branch does not exist.

```
```ucm
.> switch no-such-project

  no-such-project/main does not exist.

```
