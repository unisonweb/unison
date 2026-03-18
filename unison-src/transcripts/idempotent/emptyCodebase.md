# The empty codebase

The Unison codebase, when first initialized, contains no definitions in its namespace.

Not even `Nat` or `+`\!

BEHOLD\!\!\!

``` ucm :error
> ls .

  nothing to show
```

Technically, the definitions all exist, but they have no names. `builtins.merge` brings them into existence, under the current namespace:

``` ucm
> builtins.merge lib.builtins

  Done.

> ls lib

  1. builtins. (764 terms, 118 types)
```

And for a limited time, you can get even more builtin goodies:

``` ucm
> builtins.mergeio lib.builtinsio

  Done.

> ls lib

  1. builtins.   (764 terms, 118 types)
  2. builtinsio. (937 terms, 136 types)
```

More typically, you'd start out by pulling `base`.
