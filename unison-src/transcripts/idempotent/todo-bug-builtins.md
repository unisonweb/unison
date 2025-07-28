# The `todo` and `bug` builtin

``` ucm :hide
scratch/main> builtins.merge
```

`todo` and `bug` have type `a -> b`. They take a message or a value of type `a` and crash during runtime displaying `a` in ucm.

``` unison :error
> todo "implement me later"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

  💔💥

  I've encountered a call to builtin.todo with the following
  value:

    "implement me later"

  Stack trace:
    #kuk93g9qt6
    #qe5e1lcfn8
```

``` unison :error
> bug "there's a bug in my code"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

  💔💥

  I've encountered a call to builtin.bug with the following
  value:

    "there's a bug in my code"

  Stack trace:
    #o6nuga5ucb
    #m67hcdcoda
```

## Todo

`todo` is useful if you want to come back to a piece of code later but you want your project to compile.

``` unison
complicatedMathStuff x = todo "Come back and to something with x here"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + complicatedMathStuff : x -> r

  Run `update` to apply these changes to your codebase.
```

## Bug

`bug` is used to indicate that a particular branch is not expected to execute.

``` unison
test = match true with
    true -> "Yay"
    false -> bug "Wow, that's unexpected"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + test : Text

  Run `update` to apply these changes to your codebase.
```
