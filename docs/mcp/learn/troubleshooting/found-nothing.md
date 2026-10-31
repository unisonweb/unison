A search that finds nothing doesn't stop a transcript, but a command on a name that doesn't exist does.

- `find` and `find.all` with no results report that and carry on. Try other words, or the type.
- `view foo` and `docs foo` fail if `foo` doesn't exist (see [names not found](learn:troubleshooting/name-not-found)). If `foo` exists and has no documentation, `docs foo` says so and the transcript carries on.

The reply for a failed block shows the commands of that block and the error, but not the output of the commands before it in the same block. So put a command that might fail last, or in a block of its own, or list the names first with `find-in Foo` (see [find](learn:ucm/find)).
