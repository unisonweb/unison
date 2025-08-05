# Test that the options selector for fuzzy finding is working as expected for different argument types.

If an argument is required but doesn't have a fuzzy resolver, the command should just print the help.

``` ucm :error
-- The second argument of move.term is a 'new-name' and doesn't have a fuzzy resolver,

-- So it should print the arg parsing error.

> move.term

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    `rename.term` takes two arguments, like `rename.term oldname
    newname`.

  You can run `help move.term` for more information on using
  `move.term`.
```

If a fuzzy resolver doesn't have any options available it should print a message instead of
opening an empty fuzzy-select.

``` ucm :error
> view

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I expected at least one argument, but received none.

  You can run `help view` for more information on using `view`.
```

``` unison :hide
optionOne = 1

nested.optionTwo = 2
```

Definition args

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> debug.fuzzy-options view _

  Select a definition to view or press <esc> to cancel:
    * optionOne
    * nested.optionTwo
```

Namespace args

``` ucm
> debug.fuzzy-options find-in _

  Select a namespace or press <esc> to cancel:
    * .
    * nested
```
