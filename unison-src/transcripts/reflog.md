```ucm:hide
scratch/main> builtins.merge lib.builtins
```

First we make some changes to the codebase so there's data in the reflog.

```unison
x = 1
```
```ucm
scratch/main> add
```
```unison
y = 2
```
```ucm
scratch/main> add
scratch/main> branch /other
scratch/other> alias.term y z
newproject/main> builtins.merge lib.builtins
newproject/main> alias.type lib.builtins.Nat MyNat
```

Should see reflog entries from the current branch

```ucm
scratch/main> reflog
```

Should see reflog entries from the current project

```ucm
scratch/main> project.reflog
```


Should see reflog entries from all projects

```ucm
scratch/main> reflog.global
```
