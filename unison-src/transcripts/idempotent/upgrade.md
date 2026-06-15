# Upgrade happy path

``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
old.foo = 17
new.foo = 18
thingy = old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + new.foo : Nat
  + old.foo : Nat
  + thingy  : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/main> move old.foo lib.old.foo

  Done.

proj/main> move new.foo lib.new.foo

  Done.
```

Test tab completion and fzf options of upgrade command.

``` ucm
proj/main> debug.tab-complete upgrade ol

   old

proj/main> debug.fuzzy-options upgrade _

  Select a dependency to upgrade or press <esc> to cancel:
    * builtin
    * new
    * old

proj/main> debug.fuzzy-options upgrade old _

  Select a dependency to upgrade to or press <esc> to cancel:
    * builtin
    * new
    * old
```

``` ucm
proj/main> upgrade old new

  I upgraded old to new.

proj/main> ls lib

  1. builtin. (775 terms, 118 types)
  2. new.     (1 term)

proj/main> view thingy

  thingy : Nat
  thingy =
    use Nat +
    foo + 10
```

``` ucm :hide
proj/main> project.delete proj
```

# Upgrade sad path

``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
old.foo = 17
new.foo = +18
thingy = old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + new.foo : Int
  + old.foo : Nat
  + thingy  : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/main> move old.foo lib.old.foo

  Done.

proj/main> move new.foo lib.new.foo

  Done.
```

``` ucm :error
proj/main> upgrade old new

  I couldn't automatically upgrade old to new.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

thingy : Nat
thingy =
  use Nat +
  foo + 10

```

Resolve the error and run `update` to finish the upgrade.

``` unison
thingy = foo + +10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ thingy : Int

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
proj/upgrade-old-to-new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  I fast-forward merged proj/upgrade-old-to-new into proj/main.

  Done.

proj/main> view thingy

  thingy : Int
  thingy =
    use Int +
    foo + +10

proj/main> ls lib

  1. builtin. (775 terms, 118 types)
  2. new.     (1 term)

proj/main> branches

       Branch   Remote branch
  1.   main     
```

``` ucm :hide
proj/main> project.delete proj
```

# Upgrade sad path (showing delete on upgrade branch)

``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
old.foo = 17
new.foo = +18
thingy = old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + new.foo : Int
  + old.foo : Nat
  + thingy  : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/main> move old.foo lib.old.foo

  Done.

proj/main> move new.foo lib.new.foo

  Done.
```

``` ucm :error
proj/main> upgrade old new

  I couldn't automatically upgrade old to new.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

thingy : Nat
thingy =
  use Nat +
  foo + 10

```

Resolve the error and run `update` to finish the upgrade.

``` unison
thingy = foo + +10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ thingy : Int

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
proj/upgrade-old-to-new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  I fast-forward merged proj/upgrade-old-to-new into proj/main.

  Done.

proj/main> ls lib

  1. builtin. (775 terms, 118 types)
  2. new.     (1 term)

proj/main> ls .

  1. lib.   (776 terms, 118 types)
  2. thingy (Int)

proj/main> branches

       Branch   Remote branch
  1.   main     
```

``` ucm :hide
proj/main> project.delete proj
```

# Upgrade with old alias

``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
old.foo = 141
new.foo = 142
bar = 141
mything = old.foo + 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar     : Nat
  + mything : Nat
  + new.foo : Nat
  + old.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
myproject/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

myproject/main> move old.foo lib.old.foo

  Done.

myproject/main> move new.foo lib.new.foo

  Done.

myproject/main> upgrade old new

  I upgraded old to new.

myproject/main> view mything

  mything : Nat
  mything =
    use Nat +
    foo + 100

myproject/main> view bar

  bar : Nat
  bar = 141
```

``` ucm :hide
myproject/main> project.delete myproject
```

# Upgrade suffixifies properly

``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
old.foo = 25
new.foo = +30
a.x.x.x.x = 100
b.x.x.x.x = 100
c.y.y.y.y = old.foo + 10
d.y.y.y.y = old.foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a.x.x.x.x : Nat
  + b.x.x.x.x : Nat
  + bar       : Nat
  + c.y.y.y.y : Nat
  + d.y.y.y.y : Nat
  + new.foo   : Int
  + old.foo   : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
myproject/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

myproject/main> move old.foo lib.old.foo

  Done.

myproject/main> move new.foo lib.new.foo

  Done.
```

``` ucm :error
myproject/main> upgrade old new

  I couldn't automatically upgrade old to new.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

bar : Nat
bar =
  use Nat +
  x + c.y.y.y.y

c.y.y.y.y : Nat
c.y.y.y.y =
  use Nat +
  foo + 10

d.y.y.y.y : Nat
d.y.y.y.y =
  use Nat +
  foo + 10

```

``` ucm :hide
myproject/main> project.delete myproject
```

# Don't upgrade refs that exist in old

If `foo#old` exists in old, and `foo#new` exists in new, you might think `upgrade old new` would rewrite references to
`#old` with references to `#new`. And it will... \!\!unless\!\! `#old` still exists in new.

``` ucm :hide
foo/main> builtins.merge lib.builtin
```

``` unison
old.foo = 18
new.other = 18
new.foo = 19
mything = old.foo + old.foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mything   : Nat
  + new.foo   : Nat
  + new.other : Nat
  + old.foo   : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
foo/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

foo/main> move old.foo lib.old.foo

  Done.

foo/main> move new.other lib.new.other

  Done.

foo/main> move new.foo lib.new.foo

  Done.

foo/main> upgrade old new

  I upgraded old to new.

foo/main> view mything

  mything : Nat
  mything =
    use Nat +
    other + other
```

``` ucm :hide
foo/main> project.delete foo
```

# Rename `__N` suffix

On a successful upgrade, if the new dependency's name ends in `__N` (where `N` is a number), then we delete that suffix
(if possible). This is because commonly one incurs `__N` suffixes when installing in-progress dependencies that don't
have an associated release yet, e.g. multiple invocations of `lib.install @user/project/main`.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
dep.foo = 1
dep__2.foo = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.foo    : Nat
  + dep__2.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.foo lib.dep.foo

  Done.

scratch/main> move dep__2.foo lib.dep__2.foo

  Done.

scratch/main> upgrade dep dep__2

  I upgraded dep to dep__2 (renamed to dep).

scratch/main> ls lib

  1. builtin. (775 terms, 118 types)
  2. dep.     (1 term)
```

``` ucm :hide
scratch/main> project.delete scratch
```

It's possible the desired name is taken, though, in which case we just leave the name alone.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
dep.foo = 1
hello.foo = 2
dep__2.foo = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.foo    : Nat
  + dep__2.foo : Nat
  + hello.foo  : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.foo lib.dep.foo

  Done.

scratch/main> move hello.foo lib.hello.foo

  Done.

scratch/main> move dep__2.foo lib.dep__2.foo

  Done.

scratch/main> upgrade hello dep__2

  I upgraded hello to dep__2.

scratch/main> ls lib

  1. builtin. (775 terms, 118 types)
  2. dep.     (1 term)
  3. dep__2.  (1 term)
```

``` ucm :hide
scratch/main> project.delete scratch
```

# Upgrading more than one library at once

Two libraries can be upgraded simultaneously.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
foo_1.foo = 17
foo_2.foo = 18
bar_1.bar = 19
bar_2.bar = 20

thing = foo_1.foo + bar_1.bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar_1.bar : Nat
  + bar_2.bar : Nat
  + foo_1.foo : Nat
  + foo_2.foo : Nat
  + thing     : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo_1.foo lib.foo_1.foo

  Done.

scratch/main> move foo_2.foo lib.foo_2.foo

  Done.

scratch/main> move bar_1.bar lib.bar_1.bar

  Done.

scratch/main> move bar_2.bar lib.bar_2.bar

  Done.

scratch/main> upgrade foo_1 foo_2 bar_1 bar_2

  I upgraded foo_1 to foo_2 and bar_1 to bar_2.

scratch/main> view thing

  thing : Nat
  thing =
    use Nat +
    foo + bar

scratch/main> ls lib

  1. bar_2.   (1 term)
  2. builtin. (775 terms, 118 types)
  3. foo_2.   (1 term)
```

``` ucm :hide
scratch/main> project.delete scratch
```

If such a an upgrade fails, the branch name doesn't contain all of the dependency names, though.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
foo_1.foo = 17
foo_2.foo = 18
bar_1.bar = 19
bar_2.bar = +20

thing = foo_1.foo + bar_1.bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar_1.bar : Nat
  + bar_2.bar : Int
  + foo_1.foo : Nat
  + foo_2.foo : Nat
  + thing     : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo_1.foo lib.foo_1.foo

  Done.

scratch/main> move foo_2.foo lib.foo_2.foo

  Done.

scratch/main> move bar_1.bar lib.bar_1.bar

  Done.

scratch/main> move bar_2.bar lib.bar_2.bar

  Done.
```

``` ucm :error
scratch/main> upgrade foo_1 foo_2 bar_1 bar_2

  I couldn't automatically upgrade foo_1 to foo_2 and
  bar_1 to bar_2.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

thing : Nat
thing =
  use Nat +
  foo + bar

```

``` ucm
scratch/upgrade> ls lib

  1. bar_2.   (1 term)
  2. builtin. (775 terms, 118 types)
  3. foo_2.   (1 term)
```

``` ucm :hide
scratch/main> project.delete scratch
```

After a successful multi-lib upgrade, we do perform the same "name unmangling" step, but a sort of best-effort,
one-at-a-time way, because it's entirely possible to have collisions on the target best name (e.g. both `foo__2` and
`foo__3` want to be renamed to `foo`).

Here's an example of two mangled names becoming unmangled successfully:

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
foo.foo = 17
foo__2.foo = 18
bar.bar = 19
bar__2.bar = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar.bar    : Nat
  + bar__2.bar : Nat
  + foo.foo    : Nat
  + foo__2.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo.foo lib.foo.foo

  Done.

scratch/main> move foo__2.foo lib.foo__2.foo

  Done.

scratch/main> move bar.bar lib.bar.bar

  Done.

scratch/main> move bar__2.bar lib.bar__2.bar

  Done.

scratch/main> upgrade foo foo__2 bar bar__2

  I upgraded foo to foo__2 (renamed to foo) and
  bar to bar__2 (renamed to bar).

scratch/main> view foo bar

  lib.bar.bar : Nat
  lib.bar.bar = 20

  lib.foo.foo : Nat
  lib.foo.foo = 18
```

``` ucm :hide
scratch/main> project.delete scratch
```

And here's an example of two mangled names fighting over unmangling to the same name, where only one succeeds:

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
foo.foo = 17
foo__2.foo = 18
bar.bar = 19
foo__3.bar = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar.bar    : Nat
  + foo.foo    : Nat
  + foo__2.foo : Nat
  + foo__3.bar : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo.foo lib.foo.foo

  Done.

scratch/main> move foo__2.foo lib.foo__2.foo

  Done.

scratch/main> move bar.bar lib.bar.bar

  Done.

scratch/main> move foo__3.bar lib.foo__3.bar

  Done.

scratch/main> upgrade foo foo__2 bar foo__3

  I upgraded foo to foo__2 (renamed to foo) and bar to foo__3.

scratch/main> view foo bar

  lib.foo.foo : Nat
  lib.foo.foo = 18

  lib.foo__3.bar : Nat
  lib.foo__3.bar = 20
```

``` ucm :hide
scratch/main> project.delete scratch
```

# A couple simple cases

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
foo_1.foo = 17
foo_2.foo = 18
bar_1.bar = 19
bar_2.bar = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar_1.bar : Nat
  + bar_2.bar : Nat
  + foo_1.foo : Nat
  + foo_2.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo_1.foo lib.foo_1.foo

  Done.

scratch/main> move foo_2.foo lib.foo_2.foo

  Done.

scratch/main> move bar_1.bar lib.bar_1.bar

  Done.

scratch/main> move bar_2.bar lib.bar_2.bar

  Done.
```

Odd number of arguments:

``` ucm
scratch/main> upgrade foo_1 foo_2 bar_1

  `lib.upgrade` takes an even number of arguments.
```

Upgrading a dependency to itself:

``` ucm
scratch/main> upgrade foo_1 foo_1

  I can't upgrade foo_1 to itself!
```

Note that specifying an upgrade multiple times *is* allowed; the old-new pairs are just de-duped:

``` ucm
scratch/main> upgrade foo_1 foo_2 foo_1 foo_2

  I upgraded foo_1 to foo_2.
```

``` ucm :hide
scratch/main> project.delete scratch
```
