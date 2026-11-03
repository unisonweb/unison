Creating and switching between projects and branches. A project holds branches, and each branch is a complete copy of the project's [codebase](learn:ucm/codebase) names that you can change independently.

- `projects` lists projects and `branches` lists the branches of the current project.
- `switch myproject/main` changes the project and branch. In a [transcript](learn:ucm/transcript), starting a command line with `myproject/main>` does the same.
- `branch foo` forks the current branch into a new branch `foo`. `branch /bar foo` forks the branch `bar`.
- `project.create foo` creates a project named `foo`.
- To bring two branches back together, see [`merge`](learn:ucm/merge).

## Detail

**Starting a command on a branch that doesn't exist** creates it (except with `transcript-read-only`, see [transcripts](learn:ucm/transcript)).

**Other branch commands.** `branch.empty` makes an empty branch, `branch.rename` renames one, and `branch.squash` squashes a branch's history. `delete.branch <project/branch>` and `delete.project <project>` delete them, and need `transcript-all`. `fork <src> <dest>` copies a [namespace](learn:ucm/codebase), possibly into another branch.

**A branch's name** is written `project/branch`, as in `myproject/main`. `/bob` alone means the branch `bob` of the current project.
