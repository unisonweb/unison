`update` / `patch` (anything that a patch) ignores the namespace named "lib" at the location it's applied. This follows
the project organization convention that dependencies are put in "lib"; it's much easier to apply a patch to all of
one's own code if the "lib" namespace is simply ignored.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
foo = 100
lib.foo = 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo     : Nat
  + lib.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = 200
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Nat
      (was also named lib.foo)

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names foo

  'foo':
  Hash          Kind   Names
  #9ntnotdp87   Term   foo
```
