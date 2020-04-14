# The empty codebase

The Unison codebase, when first initialized, contains no definitions in its namespace.

Not even `Nat` or `+`!

BEHOLD!!!

```ucm:error
.> ls
```

Technically, the definitions all exist, but they have no names. `builtins.merge` brings them into existence, under the current namespace:

```ucm
.foo> builtins.merge
.foo> ls
```

And for a limited time, you can get even more builtin goodies:

```ucm
.foo> builtins.mergeio
.foo> ls
```

More typically, you'd start out by pulling `base`.
