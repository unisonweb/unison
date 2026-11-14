# Custom command aliases

You can give a command extra names with `config.set alias.<command>`. They work like the built-in aliases, and `help` lists them alongside those.

``` unison :hide
foo = 1
```

``` ucm :hide
scratch/main> update
```

``` ucm
scratch/main> config.set alias.list "l, ll"

scratch/main> config.get alias.list

  alias.list = "l ll"

scratch/main> l

  1. foo (##Nat)

scratch/main> help l

  list (or ls, dir, l, ll)
  `list`       lists definitions and namespaces in the current
               namespace.
  `list foo`   lists the 'foo' namespace.
  `list .foo`  lists the '.foo' namespace.
```

The command has to exist, and has to be given by its main name rather than an alias.

``` ucm :error
scratch/main> config.set alias.nope "x"

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I don't know of a command called `nope`.

  You can run `help config.set` for more information on using
  `config.set`.

scratch/main> config.set alias.ls "x"

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    `ls` is an alias for `list`, so use `alias.list` instead.

  You can run `help config.set` for more information on using
  `config.set`.
```

An alias can't be the name of a built-in command, or an alias that's already used for another command.

``` ucm :error
scratch/main> config.set alias.find "ls dir"

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    These are already the names of commands, so they can't be
    used as aliases: `ls`, `dir`.

  You can run `help config.set` for more information on using
  `config.set`.

scratch/main> config.set alias.find "ll"

  These aliases are already used for other commands:

    `ll` is an alias for `list`

  Remove them from those commands first, or pick different
  names.
```

Setting the aliases to an empty string removes them.

``` ucm
scratch/main> config.set alias.list ""
```

``` ucm :error
scratch/main> l

  ⚠️
  I don't know how to l. Type `help` or `?` to get help.
```
