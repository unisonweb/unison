# delete.project

```ucm
scratch/main> project.create-empty foo
scratch/main> project.create-empty bar
-- I can delete the project I'm currently on
scratch/main> delete.project scratch
foo/main> projects
-- I can delete a different project
foo/main> delete.project bar
foo/main> projects
-- I can delete the last project, a new scratch project will be created
foo/main> delete.project foo
project/main> projects
-- If the last project is scratch, a scratch2 project will be created.
scratch/main> delete.project project
scratch/main> delete.project scratch
project/main> projects
```
