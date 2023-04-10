The `branch` command creates a new branch.

First, we'll just create a loose code namespace with a term in it for later.

```unison
someterm = 18
```

```ucm
  ☝️  The namespace .some.loose.code is empty.

.some.loose.code> add

  ⍟ I've added these definitions:
  
    someterm : Nat

```
Now, the `branch` demo:

`branch` can create a branch from a different branch in the same project.

```ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
`branch` can create a branch from a different branch in a different project.

```ucm
foo/main> branch bar/topic

  Done. I've created the bar/topic branch based off foo/main

```
`branch` can create a branch from loose code.

```ucm
.some.loose.code> branch foo/topic2

  Done. I've created the foo/topic2 branch from the namespace
  .some.loose.code

```
`switch` can create a branch from nothingness, but this feature is going away soon.

```ucm
.some.loose.code> switch foo/topic3

  Done. I've created an empty branch foo/topic3
  
  Tip: Use `merge /somebranch` or `merge .path.to.code` to
       initialize this branch.

```
