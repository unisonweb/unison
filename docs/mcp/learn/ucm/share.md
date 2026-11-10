Searching Unison Share, the website where Unison projects and libraries are published, and getting code from and sending code to it.

- `share.search <query>` searches Share like the search box on its front page. A name (`List.map`) or a type (`[a] -> Nat`) finds terms and types, and a query starting with `@` finds users, organizations and projects. Add `in @user/project` to narrow it. The results of a search for definitions are numbered, each with the project and release it is from, and the numbers work in `share.docs`, `share.source` and `share.render`: `share.docs 2`.
- `share.project-search <query>` finds projects by name or summary.
- `share.releases @user/project` lists a project's releases, newest first (up to 100) with their dates, numbered, and `share.releases.detail @user/project/releases/1.2.3` (or the number from that list) shows the release notes. Use them to see what changed between versions.
- `share.docs @user/project:name` (or a number from a search) prints the type signature and the rendered documentation of a definition, like `docs` does for your own code.
- `share.source @user/project:name` prints a definition's source, and `share.render @user/project:name` its rendered documentation. `share.render @user/project` prints the project's README. Neither installs anything (to install, see [libraries](learn:ucm/libraries)).
- `pull @user/project/branch` merges a branch from Share into the current [branch](learn:ucm/branches) (see [`merge`](learn:ucm/merge) and [temporary branches](learn:ucm/temporary-branch) for when it can't finish).
- `clone @user/project/branch local/branch` copies a project from Share to a new local project.
- `push` and `push.create` publish to Share, and need `transcript-all` (see [transcripts](learn:ucm/transcript)).

## Detail

**A Share reference** is `@user/project[/branch | /releases/x.y.z][:name]`. Without a branch or release, the latest release is used, or the default branch if there is none.

**Examples:**

````
``` ucm
scratch/main> share.search [a] -> Nat in @unison/base
scratch/main> share.source @unison/base:data.List.count
```
````

**`pull`** also takes a local branch: `pull @unison/base/main my-base/topic` merges into the branch `topic` of the local project `my-base`.
