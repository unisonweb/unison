# Calling functions with alias types

``` ucm :hide
> builtins.mergeio
```

``` unison
type alias Endo a = a -> a

increment : Endo Nat
increment n = n + 1

apply : Endo Nat -> Nat
apply f = f 41

> increment 41
> apply increment
> apply (n -> n + 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  + apply     : Endo Nat -> Nat
  + increment : Endo Nat

  Run `update` to apply these changes to your codebase.

    9 | > increment 41
          ⧩
          42

    10 | > apply increment
           ⧩
           42

    11 | > apply (n -> n + 2)
           ⧩
           43
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view increment

  increment : Endo Nat
  increment n =
    use Nat +
    n + 1
```

Calls through a stored alias must work too, while the stored signature
continues to use the alias.

``` unison
> increment 43
> apply (n -> n + 3)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > increment 43
          ⧩
          44

    2 | > apply (n -> n + 3)
          ⧩
          44
```

Calling a stored alias-typed function from an entry point must not evaluate
it ahead of time as though it were a constant.

``` unison
main : '{IO} ()
main = do
  if increment 41 == 42 then () else bug "incorrect alias function result"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + main : '{IO} ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> run main

  ()
```

Editing an alias must trigger the normal stale-dependency warning when
running a definition that still depends on its previous version.

``` unison
type alias Endo a = Nat -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = Nat -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> run main

  Sorry, I don't want to run main because it depends on
  something that hasn't been committed to the codebase yet:

    main (in codebase)
    └ increment (in codebase)
      └ Endo (in file)

  You can `update` to save and propagate these changes into your
  branch.

  If you don't want that, you can run `edit main` to add main to
  the scratch file without performing an `update`.

  Then, you can try `run main` again for an up-to-date result.
```
