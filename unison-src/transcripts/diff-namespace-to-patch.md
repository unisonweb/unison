We can create a patch from the diff between two namespaces.

```ucm:hide
.> builtins.merge
```

```unison:hide
one.a = 1
one.b = 2
oneconflicts.b = 20
one.c = 3
one.d = 4
one.e = 4

two.a = 100
two.b = 200
two.c = 300
twoconflicts.c = 30
two.d = 5
two.e = 6
```

```ucm:hide
.> add
.> merge oneconflicts one
.> merge twoconflicts two
.> delete.namespace oneconflicts
.> delete.namespace twoconflicts
```

```ucm
.> find one.
.> find two.
.> diff.namespace.to-patch one two thepatch
```

A summary of the diff:

* `one.a` -> `two.a` is a normal update.
* Even though `one.b` is conflicted, both `one.b#hash1` and `one.b#hash2` get mapped to `two.b`.
* Because `two.c` is conflicted, `one.c` doesn't end up on the left-hand side of the patch.
* Oops, a similar case slipped by - `one.d` and `one.e` map to `two.d` and `two.e` respectively, but because `one.d` and
  `one.e` were aliases, we end up with a busted patch that isn't a function. This is a bug.
* Neither `one.f` nor `two.g` end up in the patch because the names `f` and `g` are not common to both namespaces.
