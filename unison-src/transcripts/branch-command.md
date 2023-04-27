The `branch` command creates a new branch.

```ucm:hide
.> builtins.merge
.> project.create foo
.> project.create bar
```

First, we'll just create a loose code namespace with a term in it for later.

```unison:hide
someterm = 18
```

```ucm
.some.loose.code> add
```

Now, the `branch` demo:

`branch` can create a branch from a different branch in the same project, from a different branch in a different
project, or from loose code. It can also create an empty branch.

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
.> branch foo/main foo/topic12

foo/main> branch bar/topic
bar/main> branch foo/main topic2
bar/main> branch foo/main /topic3
.> branch foo/main bar/topic4

.some.loose.code> branch foo/topic13
foo/main> branch .some.loose.code topic14
foo/main> branch .some.loose.code /topic15
.> branch .some.loose.code foo/topic16

foo/main> branch.empty empty1
foo/main> branch.empty /empty2
foo/main> branch.empty foo/empty3
.> branch.empty foo/empty4
```
