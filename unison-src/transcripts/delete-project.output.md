# delete.project

```ucm
.> project.create-empty foo

  🎉 I've created the project foo.

  🎨 Type `ui` to explore this project's code in your browser.
  🌏 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

  ☝️  The namespace . is empty.

.> project.create-empty bar

  🎉 I've created the project bar.

  🎨 Type `ui` to explore this project's code in your browser.
  🌏 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

  ☝️  The namespace . is empty.

.> projects

  1. bar
  2. foo

foo/main> delete.project foo

.> projects

  1. bar

```
