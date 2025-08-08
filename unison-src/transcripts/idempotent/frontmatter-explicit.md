---
autoupdate: true
---

This transcript explicitly sets the autoupdate behavior, so

this code block should show its output

``` unison
foo = ()
```

but this should error, because the definition has automatically been added after the `unison` block.

``` ucm :error
scratch/main> add

  😶

  There's nothing for me to add right now.

  Hint: I'm currently watching for definitions in .u files under
        the 〈redacted〉 directory. Make sure you've updated
        something there before using the `update` command, or
        use `load` to load a file explicitly.
```
