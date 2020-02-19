# How merging works

Suppose we have two branches, `P1` and `P2`, and a subnamespace, `foo`, which we'll refer to with `P1.foo` , `P2.foo`. This doc explains how `merge(P1,P2)` is computed, including the `merge(P1,P2).foo` subnamespace.

`LCA(P1,P2)` is the lowest common ancestor of `P1` and `P2`. To compute `merge(P1,P2)`, we:

1. Compute `LCA(P1,P2)` and do a three way merge of that level of the tree, using the algorithm below. What about the children of `P1` and `P2`? Let's just consider a child namespace `foo`. There are a few cases:
   1. `P1` and `P2` both have foo as a child namespace. Then `merge(P1,P2).foo == merge(P1.foo, P2.foo)`
   2. `P1` has `foo` as a child namespace, but `P2` does not (or vice versa). Then we have two subcases:
      1. `LCA(P1,P2)` has no `foo`. This means that `foo` child namespace was added by `P1`. The merged result for the `foo` subnamespace is just `P1.foo`.
      2. `LCA(P1,P2)` does have `foo`. This means that `P2` _deleted_ the `foo` subnamespace. The merged result for the `foo` subnamespace is then `merge(P1.foo, cons empty LCA(P1,P2).foo)`. This does a history-preserving delete of all the definitions that existed at the `LCA` point in history.
         1. Example is like if `P1` added a new definition `foo.bar = 23` after the `LCA`, then `foo.bar` will exist in the merged result, but all the definitions that existed in `foo` at the time of the `LCA` will be deleted in the result.

### Diff-based 3-way merge algorithm

Standard 3 way merge algorithm to merge `a` and `b`:

* Let `lca = LCA(a,b)`
* merged result is: `apply(diff(lca,a) <> diff(lca,b), lca)`

Relies on some diff combining operation `<>`.

### Implementation

`Causal.threeWayMerge` accepts a `(e -> e -> e -> m e)`, since the above algorithm doesn't factor nicely into diffs being combined and the applied. Internal to the function it could still reuse the existing `BranchDiff` machinery for handling the merge of stuff at the current level, but it will be doing this other thing for merging the children.

#### Notes

We debated whether to incorporate `LCA(P1,P2).foo` somehow into case 1 above. It seems not relevant, because at best it would a worse `LCA` than the `LCA(P1.foo, P2.foo)`. This is based on the observation that whenever you update a subnamespace, you have to also update the parent namespaces, up to the root, so LCAs are going to be "tighter" further down in the namespace.
