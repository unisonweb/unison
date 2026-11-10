"The following names were not found in the codebase. Check your spelling." means a name you used isn't in the current branch.

The reply lists the names:

````
The following names were not found in the codebase. Check your spelling.
  nothing
````

Things to check:

- Spelling and capitalization. [`find <part of the name>`](learn:ucm/find) lists the names that are close.
- The branch. The command ran on the project and branch of its prompt (see [transcripts](learn:ucm/transcript)). Is it the one that has the definition?
- Libraries. A definition from a [library](learn:ucm/libraries) is found by its short name only when the name is unambiguous. Otherwise use `find.all`, and the full name with the library's folder, like `lib.unison_base_7_20_0.data.List.map`.
- Definitions in the `unison` block of your transcript aren't in the branch until you run [`update`](learn:ucm/update). In particular, `view` and `find` don't see them.
- `docs foo` looks for the documentation `foo.doc`, so it reports `foo.doc` as missing when `foo` has none. A README is itself a documentation value: read it with `display README` (see [familiarize](learn:skills/familiarize)).
