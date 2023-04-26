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

`branch` can create a branch from a different branch in the same project, from a different branch in a different
project, or from loose code. It can also create an empty branch.

```ucm
foo/main> branch topic1

  Done. I've created the topic1 branch based off of main.
  
  Tip: Use `merge /topic1 /main` to merge your work back into
       the main branch.

foo/main> branch /topic2

  Done. I've created the topic2 branch based off of main.
  
  Tip: Use `merge /topic2 /main` to merge your work back into
       the main branch.

foo/main> branch foo/topic3

  Done. I've created the topic3 branch based off of main.
  
  Tip: Use `merge /topic3 /main` to merge your work back into
       the main branch.

foo/main> branch main topic4

  Done. I've created the topic4 branch based off of main.
  
  Tip: Use `merge /topic4 /main` to merge your work back into
       the main branch.

foo/main> branch main /topic5

  Done. I've created the topic5 branch based off of main.
  
  Tip: Use `merge /topic5 /main` to merge your work back into
       the main branch.

foo/main> branch main foo/topic6

  Done. I've created the topic6 branch based off of main.
  
  Tip: Use `merge /topic6 /main` to merge your work back into
       the main branch.

foo/main> branch /main topic7

  Done. I've created the topic7 branch based off of main.
  
  Tip: Use `merge /topic7 /main` to merge your work back into
       the main branch.

foo/main> branch /main /topic8

  Done. I've created the topic8 branch based off of main.
  
  Tip: Use `merge /topic8 /main` to merge your work back into
       the main branch.

foo/main> branch /main foo/topic9

  Done. I've created the topic9 branch based off of main.
  
  Tip: Use `merge /topic9 /main` to merge your work back into
       the main branch.

foo/main> branch foo/main topic10

  Done. I've created the topic10 branch based off of main.
  
  Tip: Use `merge /topic10 /main` to merge your work back into
       the main branch.

foo/main> branch foo/main /topic11

  Done. I've created the topic11 branch based off of main.
  
  Tip: Use `merge /topic11 /main` to merge your work back into
       the main branch.

.> branch foo/main foo/topic12

  Done. I've created the topic12 branch based off of main.
  
  Tip: Use `merge /topic12 /main` to merge your work back into
       the main branch.

foo/main> branch bar/topic

  Done. I've created the bar/topic branch based off foo/main.

bar/main> branch foo/main topic2

  Done. I've created the bar/topic2 branch based off foo/main.

bar/main> branch foo/main /topic3

  Done. I've created the bar/topic3 branch based off foo/main.

.> branch foo/main bar/topic4

  Done. I've created the bar/topic4 branch based off foo/main.

.some.loose.code> branch foo/topic13

  Done. I've created the foo/topic13 branch from the namespace
  .some.loose.code.

foo/main> branch .some.loose.code topic14

  Done. I've created the foo/topic14 branch from the namespace
  .some.loose.code.

foo/main> branch .some.loose.code /topic15

  Done. I've created the foo/topic15 branch from the namespace
  .some.loose.code.

.> branch .some.loose.code foo/topic16

  Done. I've created the foo/topic16 branch from the namespace
  .some.loose.code.

foo/main> branch.empty empty1

  Done. I've created an empty branch foo/empty1.
  
  Tip: Use `merge /somebranch` or `merge .path.to.code` to
       initialize this branch.

foo/main> branch.empty /empty2

  Done. I've created an empty branch foo/empty2.
  
  Tip: Use `merge /somebranch` or `merge .path.to.code` to
       initialize this branch.

foo/main> branch.empty foo/empty3

  Done. I've created an empty branch foo/empty3.
  
  Tip: Use `merge /somebranch` or `merge .path.to.code` to
       initialize this branch.

.> branch.empty foo/empty4

  Done. I've created an empty branch foo/empty4.
  
  Tip: Use `merge /somebranch` or `merge .path.to.code` to
       initialize this branch.

```
The `branch` command can't create branches named `releases/*` nor `releases/drafts/*`.

```ucm
foo/main> branch releases/1.2.3

foo/main> switch /releases/1.2.3

  foo/releases/1.2.3 does not exist.

```
```ucm
foo/main> branch releases/drafts/1.2.3

foo/main> switch /releases/drafts/1.2.3

  foo/releases/drafts/1.2.3 does not exist.

```
