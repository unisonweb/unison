A search that finds nothing doesn't stop a transcript, but a command on a name that doesn't exist does.

- `find` and `find.all` with no results report that and carry on. Ignore their hint about `debug.find.global`: it needs `transcript-all`, and it doesn't help. Try other words, or the type.
- `view foo` and `docs foo` fail if `foo` doesn't exist, and `docs foo` fails if `foo` has no documentation: it says that `foo.doc` wasn't found (see [names not found](learn:troubleshooting/name-not-found)).

The reply for a failed block shows the commands of that block and the error, but not the output of the commands before it in the same block. So put a command that might fail last, or in a block of its own, or list the names first with `find-in Foo` (see [find](learn:ucm/find)).
