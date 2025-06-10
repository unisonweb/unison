# Upgrade happy path

``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 17
lib.new.foo = 18
thingy = lib.old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      lib.new.foo : Nat
      lib.old.foo : Nat
      thingy      : Nat
```

``` ucm
proj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

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

  I upgraded old to new, and removed old.

proj/main> ls lib

  1. builtin/ (483 terms, 78 types)
  2. new/     (1 term)

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
lib.old.foo = 17
lib.new.foo = +18
thingy = lib.old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      lib.new.foo : Int
      lib.old.foo : Nat
      thingy      : Nat
```

``` ucm
proj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
proj/main> upgrade old new

  I couldn't automatically upgrade old to new. However, I've
  added the definitions that need attention to the top of
  scratch.u.

  When you're done, you can run

    upgrade.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the upgrade instead, you
  can run

    delete.branch /upgrade-old-to-new

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
thingy : Nat
thingy =
  use Nat +
  foo + 10
```

Resolve the error and commit the upgrade.

``` unison
thingy = foo + +10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      thingy : Int
```

``` ucm
proj/upgrade-old-to-new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/upgrade-old-to-new> upgrade.commit

  I fast-forward merged proj/upgrade-old-to-new into proj/main.

proj/main> view thingy

  thingy : Int
  thingy =
    use Int +
    foo + +10

proj/main> ls lib

  1. builtin/ (483 terms, 78 types)
  2. new/     (1 term)

proj/main> branches

       Branch   Remote branch
  1.   main     
```

``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 17
lib.new.foo = +18
thingy = lib.old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⊡ Previously added definitions will be ignored: lib.new.foo
    
    ⍟ These new definitions are ok to `update`:
    
      lib.old.foo : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      thingy : Nat
```

``` ucm
proj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
proj/main> upgrade old new

  I couldn't automatically upgrade old to new. However, I've
  added the definitions that need attention to the top of
  scratch.u.

  When you're done, you can run

    upgrade.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the upgrade instead, you
  can run

    delete.branch /upgrade-old-to-new

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
thingy : Nat
thingy =
  use Nat +
  foo + 10
```

Resolve the error and commit the upgrade.

``` unison
thingy = foo + +10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      thingy : Int
```

``` ucm
proj/upgrade-old-to-new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/upgrade-old-to-new> upgrade.commit

  I fast-forward merged proj/upgrade-old-to-new into proj/main.

proj/main> view thingy

  thingy : Int
  thingy =
    use Int +
    foo + +10

proj/main> ls lib

  1. builtin/ (483 terms, 78 types)
  2. new/     (1 term)

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
lib.old.foo = 141
lib.new.foo = 142
bar = 141
mything = lib.old.foo + 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      bar         : Nat
      lib.new.foo : Nat
      lib.old.foo : Nat
      mything     : Nat
```

``` ucm
myproject/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

myproject/main> upgrade old new

  I upgraded old to new, and removed old.

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
lib.old.foo = 25
lib.new.foo = +30
a.x.x.x.x = 100
b.x.x.x.x = 100
c.y.y.y.y = lib.old.foo + 10
d.y.y.y.y = lib.old.foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      a.x.x.x.x   : Nat
      b.x.x.x.x   : Nat
      bar         : Nat
      c.y.y.y.y   : Nat
      d.y.y.y.y   : Nat
      lib.new.foo : Int
      lib.old.foo : Nat
```

``` ucm
myproject/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
myproject/main> upgrade old new

  I couldn't automatically upgrade old to new. However, I've
  added the definitions that need attention to the top of
  scratch.u.

  When you're done, you can run

    upgrade.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the upgrade instead, you
  can run

    delete.branch /upgrade-old-to-new

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
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
lib.old.foo = 18
lib.new.other = 18
lib.new.foo = 19
mything = lib.old.foo + lib.old.foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      lib.new.foo   : Nat
      lib.new.other : Nat
      lib.old.foo   : Nat
      mything       : Nat
```

``` ucm
foo/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

foo/main> upgrade old new

  I upgraded old to new, and removed old.

foo/main> view mything

  mything : Nat
  mything =
    use Nat +
    other + other
```

``` ucm :hide
foo/main> project.delete foo
```
