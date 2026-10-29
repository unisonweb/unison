Some commands report "nothing to show", "I couldn't find exact type matches" or a missing name when there is nothing to report, and that counts as a failure that stops the transcript.

- `list lib` in a project with no libraries says `nothing to show`.
- A type search, `find : MyType -> Other`, with no definition of exactly that type says `I couldn't find exact type matches, resorting to fuzzy matching...` and stops, even though it then lists near matches only when run by hand.
- `docs foo` for a definition with no documentation says that `foo.doc` wasn't found (see [names not found](learn:troubleshooting/name-not-found)).

The reply for a failed block shows the commands of that block and the error, but not the output of the commands before it in the same block. So put a command that might find nothing in a block of its own, and tag the block `:error` if you expect it to fail (see [transcripts](learn:ucm/transcript)). Then the transcript carries on, and the rest of your commands run.
