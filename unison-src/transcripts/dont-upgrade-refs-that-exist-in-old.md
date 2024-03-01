If `foo#old` exists in old, and `foo#new` exists in new, you might think `upgrade old new` would rewrite references to
`#old` with references to `#new`. And it will... !!unless!! `#old` still exists in new.

```ucm:hide
.> project.create-empty foo
foo/main> builtins.merge
foo/main> move.namespace builtin lib.builtin
```

```unison
lib.old.foo = 18
lib.new.other = 18
lib.new.foo = 19
mything = lib.old.foo + lib.old.foo
```

```ucm
foo/main> add
foo/main> upgrade old new
foo/main> view mything
```
