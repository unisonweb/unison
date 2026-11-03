You are judging whether `List.chunks` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view List.chunks`, then `dependencies List.chunks` for what it calls, following the project's own helper definitions). Use `docs` and `find` (by name and by type, `find.all : Nat -> [a] -> [[a]]`) to see what base offers for cutting a list into pieces.

Rate it:

- **GREAT**: its hash equals that of a base function that does this. Check with `crypto.hash`: `crypto.hash Sha2_256 List.chunks == crypto.hash Sha2_256 <candidate>`, after finding the candidate in the base namespace.
- **PASS**: it is just a wrapper whose entire body is a call to that function, passing its arguments along (it could be eta reduced), or a short composition of base functions such as `take` and `drop` in a fold.
- **FAIL**: it is a hand-written recursive function or loop that walks the list element by element.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
