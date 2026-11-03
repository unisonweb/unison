You are judging the behavior of `Report.render` on the branch `alice` of the project `scratch` in the Unison codebase you are connected to. A previous agent merged the branch `bob` into it. You may use any tool except `learn`. Don't change the project's definitions or branches.

The intended behavior: `Report.render : [Text] -> Text` lowercases every name, sorts them, joins them with `", "` and puts `"Names: "` in front. Run a transcript with the stanza `scratch/alice> ls Report` followed by a `unison` block with watch expressions, such as `> Report.render ["Bob", "alice", "Carol"]`, whose value is printed, and check:

1. `Report.render ["Bob", "alice", "Carol"]` is `"Names: alice, bob, carol"`.
2. `Report.render ["b", "A"]` is `"Names: a, b"`, `Report.render ["Zed", "ann", "Bob", "carl"]` is `"Names: ann, bob, carl, zed"`, and `Report.render []` is `"Names: "`.
3. `Report.render ["B", "a", "C"]` is `"Names: a, b, c"`. Three wrong implementations are told apart by it: sorting the original names and lowercasing afterwards (as a merge that keeps both edits as they came does) gives `"Names: b, c, a"`, a sort of the original names without lowercasing gives `"Names: B, C, a"`, and lowercasing without sorting gives `"Names: b, a, c"`.

It passes if every check passes. If the definition cannot be found or the code doesn't typecheck, that is a FAIL.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
