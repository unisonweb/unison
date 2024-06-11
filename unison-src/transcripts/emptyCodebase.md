# The empty codebase

The Unison codebase, when first initialized, contains no definitions in its namespace.

Not even `Nat` or `+`!

BEHOLD!!!

```ucm:error
scratch/main> ls
```

Technically, the definitions all exist, but they have no names. `builtins.merge` brings them into existence, under the current namespace:

```ucm
scratch/main foo> builtins.merge
scratch/main foo> ls
```

And for a limited time, you can get even more builtin goodies:

```ucm
scratch/main foo> builtins.mergeio
scratch/main foo> ls
```

More typically, you'd start out by pulling `base`.
