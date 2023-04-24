The `switch` command switches to an existing project or branch.

```ucm:hide
.> builtins.merge
```

Setup stuff.

```unison
someterm = 18
```

```ucm
.> project.create foo
foo/main> add
foo/main> branch topic
```

Now, the demo.

```ucm
.> switch foo
.> switch foo/topic
```

```ucm:error
.> switch foo/no-such-branch
```

```ucm:error
.> switch no-such-project
```
