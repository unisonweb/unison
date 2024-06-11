`update` / `patch` (anything that a patch) ignores the namespace named "lib" at the location it's applied. This follows
the project organization convention that dependencies are put in "lib"; it's much easier to apply a patch to all of
one's own code if the "lib" namespace is simply ignored.

```ucm:hide
scratch/main> builtins.merge
```

```unison
foo = 100
lib.foo = 100
```

```ucm
scratch/main> add
```

```unison
foo = 200
```

```ucm
scratch/main> update
scratch/main> names foo
```
