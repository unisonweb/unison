# Ensure type dependencies are properly considered in slurping

https://github.com/unisonweb/unison/pull/2821

```ucm:hide
.> builtins.merge
```


Define a type.

```unison:hide
structural type Y = Y
```

```ucm:hide
.> add
```

Now, we update `Y`, and add a new type `Z` which depends on it.

```unison
structural type Z = Z Y
structural type Y = Y Nat
```

Adding should fail for BOTH definitions, `Y` needs an update and `Z` is blocked by `Y`.
```ucm:error
.> add 
-- This shouldn't exist, because it should've been blocked.
.> view Z
```
