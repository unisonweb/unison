```ucm
.> help push
  push
  The `push` command merges a local namespace into a remote
  namespace.
  `push remote local`  merges the contents of the local
                       namespace `local` into the remote
                       namespace `remote`.
  `push remote`        publishes the current namespace into the
                       remote namespace `remote`
  `push`               publishes the current namespace into the
                       remote namespace configured in
                       `.unisonConfig` with the key `GitUrl.ns`
                       where `ns` is the current namespace
  where `remote` is a git repository, optionally followed by `:`
  and an absolute remote path, such as:
  `https://github.com/org/repo`
  `https://github.com/org/repo:.some.remote.path`


.> help
  add
  `add` adds to the codebase all the definitions from the most
  recently typechecked file.
  
  add.preview
  `add.preview` previews additions to the codebase from the most
  recently typechecked file. This command only displays cached
  typechecking results. Use `load` to reparse & typecheck the
  file if the context has changed.
  
  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates
  aliases `relative1`, `relative2`, ... in the namespace
  `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases
  `.quux.foo.foo` and `.quux.bar.bar`.
  
  alias.term
  `alias.term foo bar` introduces `bar` with the same definition
  as `foo`.
  
  alias.type
  `alias.type Foo Bar` introduces `Bar` with the same definition
  as `Foo`.
  
  back (or popd)
  `back`  undoes the last `namespace` command.
  
  builtins.merge
  Adds the builtins to `builtins.` in the current namespace
  (excluding `io` and misc).
  
  builtins.mergeio
  Adds all the builtins to `builtins.` in the current namespace,
  including `io` and misc.
  
  builtins.update
  Adds all the builtins that are missing from this namespace,
  and deprecate the ones that don't exist in this version of
  Unison.
  
  copy.patch
  `copy.patch foo bar` copies the patch `bar` to `foo`.
  
  create.author
  `create.author alicecoder "Alice McGee"` creates `alicecoder`
  values in `metadata.authors` and `metadata.copyrightHolders`.
  
  debug.file
  View details about the most recent succesfully typechecked
  file.
  
  debug.history
  Dump codebase history, compatible with
  bit-booster.com/graph.html
  
  debug.numberedArgs
  Dump the contents of the numbered args state.
  
  delete
  `delete foo` removes the term or type name `foo` from the
  namespace.
  
  delete.namespace
  `delete.namespace <foo>` deletes the namespace `foo`
  
  delete.patch
  `delete.patch <foo>` deletes the patch `foo`
  
  delete.term
  `delete.term foo` removes the term name `foo` from the
  namespace.
  
  delete.term-replacement
  delete.term-replacement <patch>` removes any edit of the term
  `foo` from the patch `patch`, or the default patch if none is
  specified.
  
  delete.type
  `delete.type foo` removes the type name `foo` from the
  namespace.
  
  delete.type-replacement
  delete.type-replacement <patch>` removes any edit of the type
  `foo` from the patch `patch`, or the default patch if none is
  specified.
  
  dependencies
  List the dependencies of the specified definition.
  
  dependents
  List the dependents of the specified definition.
  
  diff.namespace
  `diff.namespace before after` shows how the namespace `after`
                                differs from the namespace
                                `before`
  
  display
  `display foo` prints a rendered version of the term `foo`.
  
  display.to
  `display.to <filename> foo` prints a rendered version of the
  term `foo` to the given file.
  
  docs
  `docs foo` shows documentation for the definition `foo`.
  
  edit
  `edit foo` prepends the definition of `foo` to the top of the
  most recently saved file.
  
  find
  `find`          lists all definitions in the current
                  namespace.
  `find foo`      lists all definitions with a name similar to
                  'foo' in the current namespace.
  `find foo bar`  lists all definitions with a name similar to
                  'foo' or 'bar' in the current namespace.
  
  find.patch (or list.patch, ls.patch)
  `find.patch`  lists all patches in the current namespace.
  
  find.verbose (or list.verbose, ls.verbose)
  `find.verbose` searches for definitions like `find`, but
  includes hashes and aliases in the results.
  
  fork (or copy.namespace)
  `fork src dest` creates the namespace `dest` as a copy of
  `src`.
  
  help (or ?)
  `help` shows general help and `help <cmd>` shows help for one
  command.
  
  help-topics (or help-topic)
  `help-topics` lists all topics and `help-topics <topic>` shows
  an explanation of that topic.
  
  history
  `history`                     Shows the history of the current
                                path.
  `history .foo`                Shows history of the path .foo.
  `history #9dndk3kbsk13nbpeu`  Shows the history of the
                                namespace with the given hash.
                                The full hash must be provided.
  
  link
  `link metadata defn` creates a link to `metadata` from `defn`.
  Use `links defn` or `links defn <type>` to view outgoing
  links, and `unlink metadata defn` to remove a link. The `defn`
  can be either the name of a term or type, multiple such names,
  or a range like `1-4` for a range of definitions listed by a
  prior `find` command.
  
  links
  `links defn`        shows all outgoing links from `defn`.
  `links defn <type>` shows all links of the given type.
  
  list (or ls)
  `list`       lists definitions and namespaces at the current
               level of the current namespace.
  `list foo`   lists the 'foo' namespace.
  `list .foo`  lists the '.foo' namespace.
  
  load
  `load`                 parses, typechecks, and evaluates the
                         most recent scratch file.
  `load <scratch file>`  parses, typechecks, and evaluates the
                         given scratch file.
  
  merge
  `merge src`      merges `src` namespace into the current namespace
  `merge src dest` merges `src` namespace into the `dest` namespace
  
  merge.preview
  `merge.preview src`      shows how the current namespace will change after a `merge src`.
  `merge.preview src dest` shows how `dest` namespace will change after a `merge src dest`.
  
  move.namespace (or rename.namespace)
  `move.namespace foo bar` renames the path `bar` to `foo`.
  
  move.patch (or rename.patch)
  `move.patch foo bar` renames the patch `bar` to `foo`.
  
  move.term (or rename.term)
  `move.term foo bar` renames `foo` to `bar`.
  
  move.type (or rename.type)
  `move.type foo bar` renames `foo` to `bar`.
  
  names
  `names foo` shows the hash and all known names for `foo`.
  
  namespace (or cd, j)
  `namespace foo.bar`   descends into foo.bar from the current
                        namespace.
  `namespace .cat.dog`  sets the current namespace to the
                        abolute namespace .cat.dog.
  
  patch
  `patch` rewrites any definitions that depend on definitions
  with type-preserving edits to use the updated versions of
  these dependencies.
  
  pull
  The `pull` command merges a remote namespace into a local
  namespace.
  `pull remote local`  merges the remote namespace `remote` into
                       the local namespace `local`.
  `pull remote`        merges the remote namespace `remote` into
                       the current namespace
  `pull`               merges the remote namespace configured in
                       `.unisonConfig` with the key `GitUrl.ns`
                       where `ns` is the current namespace, into
                       the current namespace
  where `remote` is a git repository, optionally followed by `:`
  and an absolute remote path, such as:
  `https://github.com/org/repo`
  `https://github.com/org/repo:.some.remote.path`
  
  pull-request.create (or pr.create)
  `pull-request.create base head` will generate a request to
  merge the remote repo `head` into the remote repo `base`.
  
  example: pull-request.create https://github.com/unisonweb/base https://github.com/me/unison:.libs.pr.base
  
  pull-request.load (or pr.load)
  `pull-request.load base head` will load a pull request for
  merging the remote repo `head` into the remote repo `base`,
  staging each in the current namespace (so make yourself a
  clean spot to work first).
  `pull-request.load base head dest` will load a pull request
  for merging the remote repo `head` into the remote repo
  `base`, staging each in `dest`, which must be empty.
  
  push
  The `push` command merges a local namespace into a remote
  namespace.
  `push remote local`  merges the contents of the local
                       namespace `local` into the remote
                       namespace `remote`.
  `push remote`        publishes the current namespace into the
                       remote namespace `remote`
  `push`               publishes the current namespace into the
                       remote namespace configured in
                       `.unisonConfig` with the key `GitUrl.ns`
                       where `ns` is the current namespace
  where `remote` is a git repository, optionally followed by `:`
  and an absolute remote path, such as:
  `https://github.com/org/repo`
  `https://github.com/org/repo:.some.remote.path`
  
  quit (or exit, :q)
  Exits the Unison command line interface.
  
  reflog
  `reflog` lists the changes that have affected the root
  namespace
  
  replace.term
  `replace.term <from> <to> <patch>`  Replace the term <from> in
                                      the given patch with the
                                      term <to>.
  `replace.term <from> <to>`          Replace the term <from>
                                      with <to> in the default
                                      patch.
  
  replace.type
  `replace.type <from> <to> <patch>`  Replace the type <from> in
                                      the given patch with the
                                      type <to>.
  `replace.type <from> <to>`          Replace the type <from>
                                      with <to> in the default
                                      patch.
  
  reset-root
  `reset-root .foo`                Reset the root namespace
                                   (along with its history) to
                                   that of the `.foo` namespace.
  `reset-root #9dndk3kbsk13nbpeu`  Reset the root namespace
                                   (along with its history) to
                                   that of the namespace with
                                   hash `#9dndk3kbsk13nbpeu`.
  
  run
  `run mymain`  Runs `!mymain`, where `mymain` is searched for
                in the most recent typechecked file, or in the
                codebase.
  
  test
  `test` runs unit tests for the current branch.
  
  todo
  `todo`                 lists the refactor work remaining in
                         the default patch for the current
                         namespace.
  `todo <patch>`         lists the refactor work remaining in
                         the given patch in the current
                         namespace.
  `todo <patch> [path]`  lists the refactor work remaining in
                         the given patch in given namespace.
  
  undo
  `undo` reverts the most recent change to the codebase.
  
  unlink (or delete.link)
  `unlink metadata defn` removes a link to `detadata` from
  `defn`.The `defn` can be either the name of a term or type,
  multiple such names, or a range like `1-4` for a range of
  definitions listed by a prior `find` command.
  
  update
  `update` works like `add`, except that if a definition in the
  file has the same name as an existing definition, the name
  gets updated to point to the new definition. If the old
  definition has any dependents, `update` will add those
  dependents to a refactoring session, specified by an optional
  patch.
  `update`                  adds all definitions in the .u file,
                            noting replacements in the default
                            patch for the current namespace.
  `update <patch>`          adds all definitions in the .u file,
                            noting replacements in the specified
                            patch.
  `update <patch> foo bar`  adds `foo`, `bar`, and their
                            dependents from the .u file, noting
                            any replacements into the specified
                            patch.
  
  update.preview
  `update.preview` previews updates to the codebase from the
  most recently typechecked file. This command only displays
  cached typechecking results. Use `load` to reparse & typecheck
  the file if the context has changed.
  
  view
  `view foo` prints the definition of `foo`.
  
  view.patch
  `view.patch`          Lists all the edits in the default
                        patch.
  `view.patch <patch>`  Lists all the edits in the given patch.


.> ?
  add
  `add` adds to the codebase all the definitions from the most
  recently typechecked file.
  
  add.preview
  `add.preview` previews additions to the codebase from the most
  recently typechecked file. This command only displays cached
  typechecking results. Use `load` to reparse & typecheck the
  file if the context has changed.
  
  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates
  aliases `relative1`, `relative2`, ... in the namespace
  `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases
  `.quux.foo.foo` and `.quux.bar.bar`.
  
  alias.term
  `alias.term foo bar` introduces `bar` with the same definition
  as `foo`.
  
  alias.type
  `alias.type Foo Bar` introduces `Bar` with the same definition
  as `Foo`.
  
  back (or popd)
  `back`  undoes the last `namespace` command.
  
  builtins.merge
  Adds the builtins to `builtins.` in the current namespace
  (excluding `io` and misc).
  
  builtins.mergeio
  Adds all the builtins to `builtins.` in the current namespace,
  including `io` and misc.
  
  builtins.update
  Adds all the builtins that are missing from this namespace,
  and deprecate the ones that don't exist in this version of
  Unison.
  
  copy.patch
  `copy.patch foo bar` copies the patch `bar` to `foo`.
  
  create.author
  `create.author alicecoder "Alice McGee"` creates `alicecoder`
  values in `metadata.authors` and `metadata.copyrightHolders`.
  
  debug.file
  View details about the most recent succesfully typechecked
  file.
  
  debug.history
  Dump codebase history, compatible with
  bit-booster.com/graph.html
  
  debug.numberedArgs
  Dump the contents of the numbered args state.
  
  delete
  `delete foo` removes the term or type name `foo` from the
  namespace.
  
  delete.namespace
  `delete.namespace <foo>` deletes the namespace `foo`
  
  delete.patch
  `delete.patch <foo>` deletes the patch `foo`
  
  delete.term
  `delete.term foo` removes the term name `foo` from the
  namespace.
  
  delete.term-replacement
  delete.term-replacement <patch>` removes any edit of the term
  `foo` from the patch `patch`, or the default patch if none is
  specified.
  
  delete.type
  `delete.type foo` removes the type name `foo` from the
  namespace.
  
  delete.type-replacement
  delete.type-replacement <patch>` removes any edit of the type
  `foo` from the patch `patch`, or the default patch if none is
  specified.
  
  dependencies
  List the dependencies of the specified definition.
  
  dependents
  List the dependents of the specified definition.
  
  diff.namespace
  `diff.namespace before after` shows how the namespace `after`
                                differs from the namespace
                                `before`
  
  display
  `display foo` prints a rendered version of the term `foo`.
  
  display.to
  `display.to <filename> foo` prints a rendered version of the
  term `foo` to the given file.
  
  docs
  `docs foo` shows documentation for the definition `foo`.
  
  edit
  `edit foo` prepends the definition of `foo` to the top of the
  most recently saved file.
  
  find
  `find`          lists all definitions in the current
                  namespace.
  `find foo`      lists all definitions with a name similar to
                  'foo' in the current namespace.
  `find foo bar`  lists all definitions with a name similar to
                  'foo' or 'bar' in the current namespace.
  
  find.patch (or list.patch, ls.patch)
  `find.patch`  lists all patches in the current namespace.
  
  find.verbose (or list.verbose, ls.verbose)
  `find.verbose` searches for definitions like `find`, but
  includes hashes and aliases in the results.
  
  fork (or copy.namespace)
  `fork src dest` creates the namespace `dest` as a copy of
  `src`.
  
  help (or ?)
  `help` shows general help and `help <cmd>` shows help for one
  command.
  
  help-topics (or help-topic)
  `help-topics` lists all topics and `help-topics <topic>` shows
  an explanation of that topic.
  
  history
  `history`                     Shows the history of the current
                                path.
  `history .foo`                Shows history of the path .foo.
  `history #9dndk3kbsk13nbpeu`  Shows the history of the
                                namespace with the given hash.
                                The full hash must be provided.
  
  link
  `link metadata defn` creates a link to `metadata` from `defn`.
  Use `links defn` or `links defn <type>` to view outgoing
  links, and `unlink metadata defn` to remove a link. The `defn`
  can be either the name of a term or type, multiple such names,
  or a range like `1-4` for a range of definitions listed by a
  prior `find` command.
  
  links
  `links defn`        shows all outgoing links from `defn`.
  `links defn <type>` shows all links of the given type.
  
  list (or ls)
  `list`       lists definitions and namespaces at the current
               level of the current namespace.
  `list foo`   lists the 'foo' namespace.
  `list .foo`  lists the '.foo' namespace.
  
  load
  `load`                 parses, typechecks, and evaluates the
                         most recent scratch file.
  `load <scratch file>`  parses, typechecks, and evaluates the
                         given scratch file.
  
  merge
  `merge src`      merges `src` namespace into the current namespace
  `merge src dest` merges `src` namespace into the `dest` namespace
  
  merge.preview
  `merge.preview src`      shows how the current namespace will change after a `merge src`.
  `merge.preview src dest` shows how `dest` namespace will change after a `merge src dest`.
  
  move.namespace (or rename.namespace)
  `move.namespace foo bar` renames the path `bar` to `foo`.
  
  move.patch (or rename.patch)
  `move.patch foo bar` renames the patch `bar` to `foo`.
  
  move.term (or rename.term)
  `move.term foo bar` renames `foo` to `bar`.
  
  move.type (or rename.type)
  `move.type foo bar` renames `foo` to `bar`.
  
  names
  `names foo` shows the hash and all known names for `foo`.
  
  namespace (or cd, j)
  `namespace foo.bar`   descends into foo.bar from the current
                        namespace.
  `namespace .cat.dog`  sets the current namespace to the
                        abolute namespace .cat.dog.
  
  patch
  `patch` rewrites any definitions that depend on definitions
  with type-preserving edits to use the updated versions of
  these dependencies.
  
  pull
  The `pull` command merges a remote namespace into a local
  namespace.
  `pull remote local`  merges the remote namespace `remote` into
                       the local namespace `local`.
  `pull remote`        merges the remote namespace `remote` into
                       the current namespace
  `pull`               merges the remote namespace configured in
                       `.unisonConfig` with the key `GitUrl.ns`
                       where `ns` is the current namespace, into
                       the current namespace
  where `remote` is a git repository, optionally followed by `:`
  and an absolute remote path, such as:
  `https://github.com/org/repo`
  `https://github.com/org/repo:.some.remote.path`
  
  pull-request.create (or pr.create)
  `pull-request.create base head` will generate a request to
  merge the remote repo `head` into the remote repo `base`.
  
  example: pull-request.create https://github.com/unisonweb/base https://github.com/me/unison:.libs.pr.base
  
  pull-request.load (or pr.load)
  `pull-request.load base head` will load a pull request for
  merging the remote repo `head` into the remote repo `base`,
  staging each in the current namespace (so make yourself a
  clean spot to work first).
  `pull-request.load base head dest` will load a pull request
  for merging the remote repo `head` into the remote repo
  `base`, staging each in `dest`, which must be empty.
  
  push
  The `push` command merges a local namespace into a remote
  namespace.
  `push remote local`  merges the contents of the local
                       namespace `local` into the remote
                       namespace `remote`.
  `push remote`        publishes the current namespace into the
                       remote namespace `remote`
  `push`               publishes the current namespace into the
                       remote namespace configured in
                       `.unisonConfig` with the key `GitUrl.ns`
                       where `ns` is the current namespace
  where `remote` is a git repository, optionally followed by `:`
  and an absolute remote path, such as:
  `https://github.com/org/repo`
  `https://github.com/org/repo:.some.remote.path`
  
  quit (or exit, :q)
  Exits the Unison command line interface.
  
  reflog
  `reflog` lists the changes that have affected the root
  namespace
  
  replace.term
  `replace.term <from> <to> <patch>`  Replace the term <from> in
                                      the given patch with the
                                      term <to>.
  `replace.term <from> <to>`          Replace the term <from>
                                      with <to> in the default
                                      patch.
  
  replace.type
  `replace.type <from> <to> <patch>`  Replace the type <from> in
                                      the given patch with the
                                      type <to>.
  `replace.type <from> <to>`          Replace the type <from>
                                      with <to> in the default
                                      patch.
  
  reset-root
  `reset-root .foo`                Reset the root namespace
                                   (along with its history) to
                                   that of the `.foo` namespace.
  `reset-root #9dndk3kbsk13nbpeu`  Reset the root namespace
                                   (along with its history) to
                                   that of the namespace with
                                   hash `#9dndk3kbsk13nbpeu`.
  
  run
  `run mymain`  Runs `!mymain`, where `mymain` is searched for
                in the most recent typechecked file, or in the
                codebase.
  
  test
  `test` runs unit tests for the current branch.
  
  todo
  `todo`                 lists the refactor work remaining in
                         the default patch for the current
                         namespace.
  `todo <patch>`         lists the refactor work remaining in
                         the given patch in the current
                         namespace.
  `todo <patch> [path]`  lists the refactor work remaining in
                         the given patch in given namespace.
  
  undo
  `undo` reverts the most recent change to the codebase.
  
  unlink (or delete.link)
  `unlink metadata defn` removes a link to `detadata` from
  `defn`.The `defn` can be either the name of a term or type,
  multiple such names, or a range like `1-4` for a range of
  definitions listed by a prior `find` command.
  
  update
  `update` works like `add`, except that if a definition in the
  file has the same name as an existing definition, the name
  gets updated to point to the new definition. If the old
  definition has any dependents, `update` will add those
  dependents to a refactoring session, specified by an optional
  patch.
  `update`                  adds all definitions in the .u file,
                            noting replacements in the default
                            patch for the current namespace.
  `update <patch>`          adds all definitions in the .u file,
                            noting replacements in the specified
                            patch.
  `update <patch> foo bar`  adds `foo`, `bar`, and their
                            dependents from the .u file, noting
                            any replacements into the specified
                            patch.
  
  update.preview
  `update.preview` previews updates to the codebase from the
  most recently typechecked file. This command only displays
  cached typechecking results. Use `load` to reparse & typecheck
  the file if the context has changed.
  
  view
  `view foo` prints the definition of `foo`.
  
  view.patch
  `view.patch`          Lists all the edits in the default
                        patch.
  `view.patch <patch>`  Lists all the edits in the given patch.


.> help-topics
  🌻
  
  Here's a list of topics I can tell you more about: 
  
    filestatus
    messages.disallowedAbsolute
    namespaces
    testcache
  
  Example: use `help filestatus` to learn more about that topic.


.> help filestatus
  📓
  
  Here's a list of possible status messages you might see for
  definitions in a .u file.
  
  needs update         A definition with the same name as an
                       existing definition. Doing `update`
                       instead of `add` will turn this failure
                       into a successful update.
                       
  conflicted           A definition with the same name as an
                       existing definition. Resolving the
                       conflict and then trying an `update`
                       again will turn this into a successful
                       update.
                       
  term/ctor collision  A definition with the same name as an
                       existing constructor for some data type.
                       Rename your definition or the data type
                       before trying again to `add` or `update`.
                       
  ctor/term collision  A type defined in the file has a
                       constructor that's named the same as an
                       existing term. Rename that term or your
                       constructor before trying again to `add`
                       or `update`.
                       
  blocked              This definition was blocked because it
                       dependended on a definition with a failed
                       status.
                       
  extra dependency     This definition was added because it was
                       a dependency of a definition explicitly
                       selected.


```
