The `branch` command creates a new branch.

```ucm:hide
scratch/main> project.create-empty foo
scratch/main> project.create-empty bar
```

First, we'll create a term to include in the branches.

```unison:hide
someterm = 18
```

```ucm
scratch/main> builtins.merge lib.builtins
scratch/main> add
```

Now, the `branch` demo:

`branch` can create a branch from a different branch in the same project, from a different branch in a different
project. It can also create an empty branch.

```ucm
foo/main> branch topic1
foo/main> branch /topic2
foo/main> branch foo/topic3
foo/main> branch main topic4
foo/main> branch main /topic5
foo/main> branch main foo/topic6
foo/main> branch /main topic7
foo/main> branch /main /topic8
foo/main> branch /main foo/topic9
foo/main> branch foo/main topic10
foo/main> branch foo/main /topic11
scratch/main> branch foo/main foo/topic12

foo/main> branch bar/topic
bar/main> branch foo/main topic2
bar/main> branch foo/main /topic3
scratch/main> branch foo/main bar/topic4

foo/main> branch.empty empty1
foo/main> branch.empty /empty2
foo/main> branch.empty foo/empty3
scratch/main> branch.empty foo/empty4
```

The `branch` command can create branches named `releases/drafts/*` (because why not).

```ucm
foo/main> branch releases/drafts/1.2.3
foo/main> switch /releases/drafts/1.2.3
```

The `branch` command can't create branches named `releases/*` nor `releases/drafts/*`.

```ucm:error
foo/main> branch releases/1.2.3
foo/main> switch /releases/1.2.3
```
