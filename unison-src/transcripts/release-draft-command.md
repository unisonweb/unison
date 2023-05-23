The `release.draft` command drafts a release from the current branch.

```ucm:hide
.> builtins.merge
```

Some setup:

```unison
someterm = 18
```

```ucm
.> project.create foo
foo/main> add
```

Now, the `release.draft` demo:

`release.draft` accepts a single semver argument.

```ucm
foo/main> release.draft 1.2.3
```

It's an error to try to create a `releases/drafts/x.y.z` branch that already exists.

```ucm:error
foo/main> release.draft 1.2.3
```
