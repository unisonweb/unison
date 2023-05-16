# delete.project

```ucm
.> project.create foo

  I just created project foo with branch main.

  ☝️  The namespace . is empty.

.> project.create bar

  I just created project bar with branch main.

  ☝️  The namespace . is empty.

.> projects

  1. bar
  2. foo

foo/main> delete.project foo

.> projects

  1. bar

```
