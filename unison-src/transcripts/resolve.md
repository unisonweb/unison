# Resolving edit conflicts in `ucm`

```ucm:hide
.> builtins.merge
```

The `ucm` tool tracks edits to hashes in an object called a _patch_. When patches get merged, sometimes those patches will have conflicting edits. The `replace` command helps resolve such conflicts.

First, let's make a new namespace, `example.resolve`:

```ucm
.> cd example.resolve
```

Now let's add a term named `a.foo`:

```unison
a.foo = 42
```

```ucm
.example.resolve> add
```

We'll fork the namespace `a` into a new namespace `b`, so we can edit the two concurrently.

```ucm
.example.resolve> fork a b
```

We'll also make a second fork `c` which we'll use as the target for our patch later.

```ucm
.example.resolve> fork a c
```

Now let's make a change to `foo` in the `a` namespace:

```ucm
.example.resolve> cd a
```

```unison
foo = 43
```

```ucm
.example.resolve.a> update
```

And make a different change in the `b` namespace:

```ucm
.example.resolve> cd .example.resolve.b
```

```unison
foo = 44
```

```ucm
.example.resolve.b> update
```

The `a` and `b` namespaces now each contain a patch named `patch`. We can view these:

```ucm
.example.resolve.b> cd .example.resolve
.example.resolve> view.patch a.patch
.example.resolve> view.patch b.patch
```

Let's now merge these namespaces into `c`:

```ucm
.example.resolve> merge a c
```
```ucm:error
.example.resolve> merge b c
```

The namespace `c` now has an edit conflict, since the term `foo` was edited in two different ways.

```ucm:error
.example.resolve> cd c
.example.resolve.c> todo
```

We see that the original hash of `a.foo` got replaced with _two different_ hashes.

We can resolve this conflict by picking one of the terms as the "winner":

```ucm
.example.resolve.c> replace 1 2
```

This changes the merged `c.patch` so that only a single edit remains and resolves the conflict.

```ucm
.example.resolve.c> view.patch
```

We still have a remaining _name conflict_ since it just so happened that both of the definitions in the edits were named `foo`.

```ucm:error
.example.resolve.c> todo
```

We can resolve the name conflict by deleting one of the names.

```ucm
.example.resolve.c> delete.term 2
.example.resolve.c> todo
```

And that's how you resolve edit conflicts with UCM.
