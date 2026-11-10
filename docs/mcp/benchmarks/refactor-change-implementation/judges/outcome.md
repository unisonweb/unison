You are judging the end state of the project in the Unison codebase you are connected to. A previous agent was asked to change `Text.slug : Text -> Text` so that runs of dashes collapse into one and dashes at the start or end are dropped, keeping everything that uses it working, and to add tests for the new cases. You may use any tool except `learn`. Don't change anything in the codebase (no `update`, `switch` or edits); read it with transcripts that only run read commands, plus a `unison` block of watch expressions where this says so.

Setup, so you know what to expect: the project `scratch`, branch `main`, had `Text.slug` with its documentation `Text.slug.doc` and the test `Text.slug.tests.basic`; its users `Post.url`, `Post.urls`, `Post.anchor` and `Site.index`, with the tests `Post.tests.url` and `Site.tests.index`; and `Text.initials` with `Text.tests.initials`, which don't use `slug`. The change keeps `slug`'s type, so nothing else needed to change.

Run these and check each:

1. `scratch/main> branches` lists exactly `main`, and `scratch/main> todo` has nothing pending.
2. `scratch/main> find` lists every definition named above, `Text.slug.doc` included, and no other definition apart from new tests (names containing `tests`): no leftover helper or copy of `slug` under another name (a helper that `slug` uses is fine if it is documented and under `Text.slug` or `Text`, say so).
3. `scratch/main> view Text.slug` shows `Text.slug : Text -> Text`.
4. A `unison` block of watch expressions:

   ````
   > Text.slug "  Hello --  World! "
   > Text.slug "a - b"
   > Text.slug "Hello World"
   > Text.slug "--x--"
   > Post.url "My First Post"
   ````

   must give `"hello-world"`, `"a-b"`, `"hello-world"`, `"x"` and `"/posts/my-first-post"`.
5. `scratch/main> test` runs and every test passes, the four original tests are still there (`scratch/main> find tests`), and at least one test (new, or added to `Text.slug.tests.basic`) checks a collapsed run of dashes and a dropped leading or trailing dash, with the expected values written out.

It passes if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
