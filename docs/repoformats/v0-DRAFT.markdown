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
  dependents/_builtin/<dependent-hash>

  patches/<hash>.up -- patches, linked from the branches
```

Something that still needs working out:

* It's quite possible to have multiple definitions with the same hash, if Alice and Bob implement a thing with the same hash independently.
* To avoid git conflicts, we need to tweak the repo format. Perhaps name each definition according to a hash of its content, as in:
  * `terms/<hash>/<content-hash>.compiled.ub
  * `terms/<hash>/<content-hash>.type.ub
* That sort of change has some implications that we'll need to work through - like maybe the branches should not have `Reference` in them, but `Reference` + `<content-hash>`? TBD.
