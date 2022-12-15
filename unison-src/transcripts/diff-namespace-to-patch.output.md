We can create a patch from the diff between two namespaces.

```unison
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

```ucm
.> find one.

  1. one.a : Nat
  2. one.b#cp6 : Nat
  3. one.b#dcg : Nat
  4. one.c : Nat
  5. one.d : Nat
  

.> find two.

  1. two.a : Nat
  2. two.b : Nat
  3. two.c#k86 : Nat
  4. two.c#qpo : Nat
  5. two.d : Nat
  6. two.e : Nat
  

.> diff.namespace.to-patch one two thepatch

  Edited Terms:
    1. one.b#cp6ri8mtg0 -> 4. two.b
    2. one.b#dcgdua2lj6 -> 5. two.b
    3. one.a            -> 6. two.a
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
A summary of the diff:

* `one.a` -> `two.a` is a normal update.
* Even though `one.b` is conflicted, both `one.b#hash1` and `one.b#hash2` get mapped to `two.b`.
* Because `two.c` is conflicted, `one.c` doesn't end up on the left-hand side of the patch.
* Because `one.d` and `one.e` are aliases, they don't end up on the left-hand side of the patch.
* Neither `one.f` nor `two.g` end up in the patch because the names `f` and `g are not common to both namespaces.
