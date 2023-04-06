Deleting the branch you are on takes you to its parent (though this is impossible to see in a transcript, since we set
your working directory with each command).

```ucm
.> project.create foo
foo/main> switch /topic
foo/topic> delete.branch /topic
```

You can delete the only branch in a project.

```ucm
foo/main> delete.branch /main
```
