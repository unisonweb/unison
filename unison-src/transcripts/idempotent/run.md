The `run` command executes something of type `'{IO, Exception} a` and prints the result.

Here's a simple example of running something in the codebase.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = do 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run foo

  1
```

``` ucm :hide
scratch/main> project.delete scratch
```

The scratch file contents are prioritized over the codebase.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = do 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = do 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : 'Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run foo

  2
```

``` ucm :hide
scratch/main> project.delete scratch
```

If you try to run something of an incompatible type, you'll get an error.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> run foo

  😶

  I found this function:

    foo : Nat

  but in order for me to `run` it needs to be a subtype of:

    foo : '{IO, Exception} result
```

``` ucm :hide
scratch/main> project.delete scratch
```

There's a "staleness check" that works as follows. If you try to `run x`, where `x` isn't hash-qualified, and where `x`
(either found in the scratch file or codebase) depends on a type or term `y` that has registered as a pending update in
the latest typechecked Unison file, then we refuse to run `x`.

The logic is that the user *probably* wants to see their updated `y` (which isn't yet committed to the codebase)
reflected in their run of `x`.

In this example, `foo` depends on `bar` which depends on `baz`. The user first puts all of them in their scratch file,
then tries to run `baz`, which works fine.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = do 1
bar = do foo() + 1
baz = do bar() + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : 'Nat
  + baz : 'Nat
  + foo : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run baz

  3
```

The user then stages an update to `foo` in their scratch file, but because `bar` and `baz` are also in the scratch file,
there isn't a dependency from `baz` on the old `foo`, so the run works fine.

``` unison
foo = do 2
bar = do foo() + 1
baz = do bar() + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ bar : 'Nat
  ~ baz : 'Nat
  ~ foo : 'Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run baz

  4
```

However, the user then deletes `bar` from their scratch file, and now the `baz` in the scratch file depends on the `bar`
in the *codebase*, which depends on the `foo` in the *codebase*, which is shadowed by the updated `foo` in the scratch
file. In this case, `run baz` doesn't work.

``` unison
foo = do 2
baz = do bar() + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : 'Nat

  (and 1 unchanged term)

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> run baz

  Sorry, I don't want to run baz because it depends on something
  that hasn't been committed to the codebase yet:

    baz (in file)
    └ bar (in codebase)
      └ foo (in file)

  You can `update` to save and propagate these changes into your
  branch.

  If you don't want that, you can run `edit.dependents foo` to
  add all callers of foo to the scratch file without performing
  an `update`.

  Then, you can try `run baz` again for an up-to-date result.
```

``` ucm :hide
scratch/main> project.delete scratch
```

This example demonstrates the same error as above, but by trying to run a term in the codebase, not the scratch file.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = do 1
bar = do foo() + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : 'Nat
  + foo : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = do 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : 'Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> run bar

  Sorry, I don't want to run bar because it depends on something
  that hasn't been committed to the codebase yet:

    bar (in codebase)
    └ foo (in file)

  You can `update` to save and propagate these changes into your
  branch.

  If you don't want that, you can run `edit bar` to add bar to
  the scratch file without performing an `update`.

  Then, you can try `run bar` again for an up-to-date result.
```

``` ucm :hide
scratch/main> project.delete scratch
```

And this example demonstrates the same error as above, but via an uncommitted type, not term.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
type A = A
foo = do A
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type A

  + foo : 'A

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type A = A ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type A

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> run foo

  Sorry, I don't want to run foo because it depends on something
  that hasn't been committed to the codebase yet:

    foo (in codebase)
    └ A (in file)

  You can `update` to save and propagate these changes into your
  branch.

  If you don't want that, you can run `edit foo` to add foo to
  the scratch file without performing an `update`.

  Then, you can try `run foo` again for an up-to-date result.
```

``` ucm :hide
scratch/main> project.delete scratch
```
