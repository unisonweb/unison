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

`branch` can create a branch from a different branch in the same project.

```ucm
foo/main> branch topic
```

`branch` can create a branch from a different branch in a different project.

```ucm
foo/main> branch bar/topic
```

`branch` can create a branch from loose code.

```ucm
.some.loose.code> branch foo/topic2
```

`switch` can create a branch from nothingness, but this feature is going away soon.

```ucm
.some.loose.code> switch foo/topic3
```
