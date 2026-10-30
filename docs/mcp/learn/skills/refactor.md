Changing a definition that other definitions use (adding or swapping an argument, renaming, changing a type) means changing every use. Let UCM find the uses and rewrite them where it can, and edit by hand only what is left.

- **Find the uses.** `dependents <name>` lists what uses a definition, and `edit.dependents <name>` puts all of them (and everything that uses those) in the scratch file. See [find](learn:ucm/find).
- **Find uses by shape with `rewrite.find`.** It takes a rewrite, a `@rewrite` block, and lists every definition in the branch that matches the left side of any of its rules. The right side is ignored, so a rule like `x = @rewrite term foo a b ==> ()` finds every call of `foo` with two arguments.
- **Change them with `rewrite`.** The same rule, with a real right side, rewrites the matches. It changes nothing in the branch: it writes the rewritten definitions to a new file, `rewrites/<time>-<guid>.u`, named in the reply. Read the file, `load` it to typecheck it (a plain `load` loads the latest file), fix what is left by hand, and `update` to save. See the detail below for the exact steps.
- **Rules.** `term a b ==> c` rewrites an expression (`a` and `b` stand for any expression), `case P ==> Q` rewrites a pattern in a `match` or `cases`, and `signature a . T ==> U` rewrites a type in a signature. One `@rewrite` block can have several.
- **A rewrite can't define anything.** A helper that the right side uses has to be in the branch, or you add it to the file `rewrite` produced. A rewrite can't add a new case to a handler or a new constructor to a match, so those you edit by hand.
- When `update` can't finish because definitions stopped typechecking, see [update](learn:ucm/update) and [temporary branches](learn:ucm/temporary-branch): the file UCM writes has the dependents in it.

## Detail

**An example.** `bullets : [Text] -> Text` should take a limit first, and every use should pass `maxNat`. First define the new function and the rule in a `unison` block (they are typechecked, not saved), then find and rewrite:

````
``` unison
bulletsLimit : Nat -> [Text] -> Text
bulletsLimit limit items = ...

addLimit = @rewrite term bullets ==> bulletsLimit maxNat
```

``` ucm
myproject/main> rewrite.find :addLimit
myproject/main> rewrite :addLimit
```
````

`:addLimit` means a rewrite named `addLimit` in the latest typechecked file, or else in the branch. `rewrite.find` numbers its list, so `edit 1-3` brings those into the scratch file if you would rather edit them by hand. The file `rewrite` wrote has the changed definitions. Add the new `bullets` to it (and rename `bulletsLimit` in it), `load` it, and `update`.

**Where the rewrites come from.** `foo.u:r1,r2` takes `r1` and `r2` from the file `foo.u`; a file with one definition can be given alone, `foo.u`. `:r1,r2` takes them from the latest typechecked file, else the branch.

**What to rewrite.** With no second argument, `rewrite` looks at the whole branch (not `lib`). `rewrite :r1 path.to.module` limits it to a namespace, and `rewrite :r1 foo.u` rewrites the definitions in a file instead, writing `rewrites/foo-<time>-<guid>.u` next to it.

**Check what changed** before you `update`: `load` shows which definitions changed, and the tests in the file run.
