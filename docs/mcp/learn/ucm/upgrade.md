Upgrading a library you depend on to a newer version, with `upgrade`.

- Install the new version next to the old one with `lib.install` (see [libraries](learn:ucm/libraries)), so that `lib` has both, for example `lib.old` and `lib.new`.
- `upgrade old new` switches everything in your code that uses `lib.old` to use `lib.new`, then removes `lib.old`. `lib.new` stays under its own name.
- If some of your code no longer typechecks against the new version, the upgrade can't finish. You are put on a [temporary branch](learn:ucm/temporary-branch), `upgrade-old-to-new`, with a file of the code to fix.
- `upgrade` takes more than one pair: `upgrade old new old2 new2`.

## Detail

**Another name for it** is `lib.upgrade`.

**An example.** `thingy` uses `old.foo`, and the new version of the library has `foo` with a different type:

````
``` ucm
myproject/main> upgrade old new
```
````

`thingy` no longer typechecks, so it is in the file `upgrade-old-to-new-<hash>.u` on the [temporary branch](learn:ucm/temporary-branch). Fix it there, then [`load` and `update`](learn:ucm/scratch-file).

**When it works,** the reply says `I upgraded old to new`, and `list lib` shows `new` and no `old`.
