The codebase is the database where Unison keeps your code. It stores definitions by a hash of their content, and keeps the names for them separately, organized into namespaces, projects and branches.

- A **definition** is a term (a value or function) or a type. It is identified by a **hash** of its content, so a definition with a given hash never changes.
- A **name** is just a label for a definition (a hash) in a [branch](learn:ucm/branches). One definition can have several names, and a name can be changed or moved without changing the definition.
- Names live in a tree of **namespaces**, separated by dots: `List.map` is `map` in the namespace `List`. Names are relative to the current namespace.
- A [project](learn:ucm/branches) has branches, and each branch has its own tree of names. A project's libraries are under the namespace `lib` (see [libraries](learn:ucm/libraries)).
- Code you write in a [`unison` block](learn:ucm/scratch-file) isn't in the codebase until you run [`update`](learn:ucm/update).

## Detail

**Why this matters.** Because anything that uses a definition refers to its hash, renaming or moving a definition doesn't break anything that uses it, and changing a definition produces a new one: [`update`](learn:ucm/update) then has to point everything that used the old one at the new one, which may not typecheck (see [temporary branches](learn:ucm/temporary-branch)).

**Reading names.** `lib.base.List.map` is `List.map` inside the library `base`, under `lib`. A name like `#abc123` is a hash, used where a definition needs to be named without ambiguity, as in [`reset`](learn:ucm/history) and `names`.

**Several names, one definition.** [`alias.term`](learn:ucm/rename-and-delete) adds a name for a definition, and [`find`](learn:ucm/find) and `names <name>` show all the names a definition has.
