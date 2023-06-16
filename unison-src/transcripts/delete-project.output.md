# delete.project

```ucm
.> project.create foo

  🎉 I've created the project foo.

  ☝️  The namespace . is empty.

.> project.create bar

  🎉 I've created the project bar.

  ☝️  The namespace . is empty.

.> projects

  1. bar
  2. foo

foo/main> delete.project foo

.> projects

  1. bar

```
