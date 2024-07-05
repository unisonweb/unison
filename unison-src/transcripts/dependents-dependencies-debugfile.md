```ucm:hide
scratch/main> builtins.merge
```

### `debug.file`
I can use `debug.file` to see the hashes of the last typechecked file.

Given this .u file:
```unison:hide
structural type outside.A = A Nat outside.B
structural type outside.B = B Int
outside.c = 3
outside.d = c < (p + 1)

structural type inside.M = M outside.A
inside.p = c
inside.q x = x + p * p
inside.r = d
```
```ucm
scratch/main> debug.file
```

This will help me make progress in some situations when UCM is being deficient or broken.

### `dependents` / `dependencies`
But wait, there's more.  I can check the dependencies and dependents of a definition:
```ucm
scratch/main> add
scratch/main> dependents q
scratch/main> dependencies q
scratch/main> dependencies B
scratch/main> dependencies d
scratch/main> dependents d
scratch/main>
```

We don't have an index for dependents of constructors, but iirc if you ask for that, it will show you dependents of the structural type that provided the constructor.
