
```ucm:hide
.> builtins.merge
```

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

```unison:hide
foo.w = 2
foo.x = 1
baz.x = 3
quux.x = 4
```

```ucm
.P0> add
```

Now P0 has 3 sub-namespaces.
* foo will be modified definition-wise in each branch
* baz will be deleted in the P2 branch and left alone in P1
* quux will be deleted in the P2 branch and added to in P1
* P1 will add a bar sub-namespace

```ucm
.P0> fork .P0 .P1
.P0> fork .P0 .P2
```

```unison:hide
foo.y = 2483908
bar.y = 383
quux.y = 333
```

```ucm
.P1> add
.P1> delete.term.verbose foo.w
```

We added to `foo`, `bar` and `baz`, and deleted `foo.w`, which should stay deleted in the merge.

```unison:hide
foo.z = +28348
```

```ucm
.P2> add
.P2> delete.namespace baz
.P2> delete.namespace quux
.P2> find
```

We added `foo.z`, deleted whole namespaces `baz` and `quux` which should stay
deleted in the merge.

Now we'll try merging `P1` and `P2` back into `P0`. We should see the union of all their definitions in the merged version of `P0`.

This should succeed and the resulting P0 namespace should have `foo`, `bar`
and `quux` namespaces.

```ucm
.P0> merge .P1
.P0> merge .P2
.P0> find
.P0> view foo.x foo.y foo.z bar.y quux.y
```

These test that things we expect to be deleted are still deleted.

```ucm:error
.> view P0.foo.w
```

```ucm:error
.> view P0.baz.x
```

```ucm:error
.> view P0.quux.x
```

### Corner cases

We're going to now do two concurrent edits with an update on one side to make sure 3-way merge behaves as expected.

Here's the starting namespace, which will be the LCA.

```unison:hide
a = 1

f = (x y -> y) a "woot!"
```

```ucm
.c1> add
.> fork c1 c1a
.> fork c1 c1b
```

```unison:hide
oog.b = 230948
oog.c = 339249
```

In `c1a`, we add new definitions, `b` and `c`.

```ucm
.c1a> add
```

In `c1b`, we update the definition `a`, which is used by `f`.

```unison:hide
a = "hello world!"
```

```ucm
.c1b> update
```

Now merging `c1b` into `c1a` should result in the updated version of `a` and `f`, and the new definitions `b` and `c`:

```ucm
.> merge c1b c1a
.c1a> todo .c1b.patch
.c1a> find
.c1a> view 1-4
```
