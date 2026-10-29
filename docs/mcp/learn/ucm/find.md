Searching for definitions in the [codebase](learn:ucm/codebase), and reading their source, types and documentation.

- `find <name>` lists definitions with a similar name. It leaves out libraries (`lib`, see [libraries](learn:ucm/libraries)), so use `find.all <name>` to include them.
- `view <name>` (also `source <name>`) shows the source of a definition. `signature <name>` shows its type, and `docs <name>` shows its type signature (or the heading of a type) followed by its documentation. `display <name>` (also `render <name>`) shows the rendered value of a term, such as a README or other `Doc`.
- `names <name>` lists all the names a definition has.
- `dependencies <name>` lists what a definition uses, and `dependents <name>` what uses it.
- `list [namespace]` lists the definitions and sub-namespaces in a [namespace](learn:ucm/codebase).

## Detail

**More ways to search.** `find` with no argument lists everything in the current [namespace](learn:ucm/codebase), `find-in <namespace> [names]` searches a sub-namespace, and `find.all` and `find-in.all` include one level of `lib`. `text.find <token>` finds terms with text or number literals containing the token (quote numbers: `"42"`). `view.global <name>` looks across the whole codebase, not just the current branch.

**View several at once.** `view` takes several names, and `?` is a wildcard: `view List.?`.

**Put code in the scratch file.** `edit <name>` puts a definition's source in the [scratch file](learn:ucm/scratch-file) so you can change it, and `edit.dependents <name>` includes everything that depends on it.

**Problems in the branch.** `todo` lists conflicted names, dependencies with missing names, and other problems in the branch.

**Searching Share.** `find` only searches your own [branch](learn:ucm/branches). To search by name or by type across [Unison Share](learn:ucm/share), use `share.search`.
