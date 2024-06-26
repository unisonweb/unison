Deleting the branch you are on takes you to its parent (though this is impossible to see in a transcript, since we set
your working directory with each command).

```ucm
foo/main> branch topic
foo/topic> delete.branch /topic
```

A branch need not be preceded by a forward slash.

```ucm
foo/main> branch topic
foo/topic> delete.branch topic
```

You can precede the branch name by a project name.

```ucm
foo/main> branch topic
scratch/main> delete.branch foo/topic
```

You can delete the only branch in a project.

```ucm
foo/main> delete.branch /main
```
