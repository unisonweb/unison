Searching for definitions in the [codebase](learn:ucm/codebase), and reading their source, types and documentation.

- `find <name>` lists definitions with a similar name. It leaves out libraries (`lib`, see [libraries](learn:ucm/libraries)), so use `find.all <name>` to include them.
- `view <name>` (also `source <name>`) shows the source of a definition, with its documentation if it has any, so use it to read code. `docs <name>` says so when there is no documentation, and the transcript carries on. `signature <name>` shows its type, and `docs <name>` shows its type signature (or the heading of a type) followed by its documentation. `display <name>` (also `render <name>`) shows the rendered value of a term, such as a README or other `Doc`.
- `names <name>` lists all the names a definition has.
- `dependencies <name>` lists what a definition uses, and `dependents <name>` what uses it.
- `list [namespace]` (also `ls`) lists one level of a [namespace](learn:ucm/codebase). Write `ls Foo`, not `ls Foo.`.
- **To read a whole area, use one call, not one for each name:** `find-in Foo` lists everything under `Foo` at any depth, including `Foo.bar.tests.baz`, and numbers it. `view 1-15` then shows those definitions, each with its documentation.
- `foo.tests` is usually a namespace and not a definition, so `view foo.tests` fails: `find-in foo.tests` lists the tests in it. There are no wildcards: `view Foo.*` and `view Foo.?` fail.

## Detail

**More ways to search.** `find` with no argument lists everything in the current [namespace](learn:ucm/codebase), `find-in <namespace> [names]` searches a sub-namespace, and `find.all` and `find-in.all` include one level of `lib`. When a search finds nothing, try other words or the type. `text.find <token>` finds terms with text or number literals containing the token (quote numbers: `"42"`). `view.global <name>` looks across the whole codebase, not just the current branch.

**View several at once.** `view` takes several names and numbers from the last list: `view foo bar`, `view 1-5`. `docs` takes a number too.

**Put code in the scratch file.** `edit <name>` puts a definition's source in the [scratch file](learn:ucm/scratch-file) so you can change it, and `edit.dependents <name>` includes everything that depends on it. `find`, `rewrite.find`, `dependents` and similar commands number their results, and `edit` takes the numbers: `edit 1`, or `edit 1-5` for a range, so you can dump some or all of a list into the file you are working on. `edit.to <file> <names>` (and `edit.new.to`, `edit.namespace.to`, `edit.dependents.to`) sends them to a `.u` file of your choice instead, a real file that it creates if needed and adds to the top of if it exists, so repeated `edit.to` calls collect definitions in one file; it refuses to write into an existing file whose name doesn't end in `.u`. Numbers can be open at the end: `edit.to work.u 1-` takes everything from the first result on.

**Find code by shape.** `dependents foo` lists every definition that uses `foo`. For a shape rather than a name, `rewrite.find` lists the definitions that match a structural pattern: the calls of a function in one particular form, every match on a constructor, every handler of an ability's operation. See [rewrite](learn:ucm/rewrite), which also changes what it finds, and [refactoring](learn:skills/refactor) for the procedures.

**Problems in the branch.** `todo` lists conflicted names, dependencies with missing names, and other problems in the branch.

**Searching Share.** `find` only searches your own [branch](learn:ucm/branches). To search by name or by type across [Unison Share](learn:ucm/share), use `share.search`.
