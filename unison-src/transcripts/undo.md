# Undo

Undo should pop a node off of the history of the current branch.

```unison:hide
x = 1
```

```ucm
scratch/main> builtins.merge lib.builtins
scratch/main> add
scratch/main> ls
scratch/main> alias.term x y
scratch/main> ls
scratch/main> history
scratch/main> undo
scratch/main> ls
scratch/main> history
```

---

It should not be affected by changes on other branches.

```unison:hide
x = 1
```

```ucm
scratch/branch1> builtins.merge lib.builtins
scratch/branch1> add
scratch/branch1> ls
scratch/branch1> alias.term x y
scratch/branch1> ls
scratch/branch1> history
-- Make some changes on an unrelated branch
scratch/branch2> builtins.merge lib.builtins
scratch/branch2> delete.namespace lib
scratch/branch1> undo
scratch/branch1> ls
scratch/branch1> history
```

---

Undo should be a no-op on a newly created branch

```ucm:error
scratch/main> branch.create-empty new
scratch/new> undo
```
