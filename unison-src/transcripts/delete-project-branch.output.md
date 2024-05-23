Deleting the branch you are on takes you to its parent (though this is impossible to see in a transcript, since we set
your working directory with each command).

```ucm
.> project.create-empty foo

  🎉 I've created the project foo.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

foo/topic> delete.branch /topic

```
A branch need not be preceded by a forward slash.

```ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

foo/topic> delete.branch topic

```
You can precede the branch name by a project name.

```ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

  ☝️  The namespace . is empty.

.> delete.branch foo/topic

```
You can delete the only branch in a project.

```ucm
foo/main> delete.branch /main

```
