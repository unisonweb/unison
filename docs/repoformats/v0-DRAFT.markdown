Documentation of the Unison codebase repo format. DRAFT, still evolving. We'll freeze this file and remove the DRAFT from the name once we do a release.

```
.unison/v0/
  branches/head/<branch-hash>
  branches/<branch-hash>.ub

  terms/<hash>/compiled.ub
  terms/<hash>/type.ub

  types/<hash>/compiled.ub -- the type declaration

  watches/test/<source-hash>.ub  -- contents have the result of evaluating source-hash
  watches/cache/<source-hash>.ub -- ditto, for other kinds of watches

  dependents/<hash>/<dependent-hash> 
  dependents/_builtin/<builtin-name>/<dependent-hash>

  patches/<hash>.up -- patches, linked from the branches
```

`dependents` is updated in the following way:

* When a definition `d` is added, we compute its hash, `h`, and the dependencies of `d`. For each `dep` in the dependencies of `d`, we `h` to the `dependents` directory, under `dependents/dep/h`. `h` is just an empty file, a link to a definition.
* Note: the dependencies of a term `d` includes the dependencies of `d`'s _type_.
* Note: git merge of `dependents` does the right thing

Something that still needs working out:

* It's quite possible to have multiple definitions with the same hash, if Alice and Bob implement a thing with the same hash independently.
* To avoid git conflicts, we need to tweak the repo format. Perhaps name each definition according to a hash of its content, as in:
  * `terms/<hash>/<content-hash>.compiled.ub`
  * `terms/<hash>/<content-hash>.type.ub`
* That sort of change has some implications that we'll need to work through - like maybe the branches should not have `Reference` in them, but `Reference` + `<content-hash>`? TBD.

When pulling from a remote repo, can just shallow clone to a temporary directory, and then file merge the contents of that directory with the user's codebase directory. One fixup is that the head of the temp directory should be ignored. It's up to the caller of the pull to decide what it wants to do with the `Branch` it gets back.

Idea is that when pulling, you always end up with a codebase where the `Branch` links to definitions are valid.

What about for pushing a branch? Example: `> merge mylibrary.v1 https://github.com/runarorama/unison`

* Option 1 (full syncing): could write the branch and its transitive dependents into a temporary directory which is a valid codebase repo. Then just push that via git.
* Option 1a (local selective syncing): Clone/pull the remote repo in some temporary directory, then do selective sync of needed files into the temporary directory. Then do a commit to the temp repo, followed by push. By recycling temporary directories, we lean on git to do selective syncing of history. The temp directories can still be deleted at any time.
* Option 2 (remote selective syncing): Remote selective syncing, start from the root, check if the root hash already exists on the remote, if yes, stop. If no, copy that file into the temporary directory. Also check if all of the hashes referenced by the root exist on the remote. If no, copy the `terms/`, `dependents`, and/or `types/` directories into the temp. Repeat for depenents.
