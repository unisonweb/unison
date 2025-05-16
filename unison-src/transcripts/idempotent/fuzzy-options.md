# Test that the options selector for fuzzy finding is working as expected for different argument types.

If an argument is required but doesn't have a fuzzy resolver, the command should just print the help.

``` ucm :error
-- The second argument of move.term is a 'new-name' and doesn't have a fuzzy resolver

scratch/main> move.term

  `move.term foo bar` renames `foo` to `bar`.
```

If a fuzzy resolver doesn't have any options available it should print a message instead of
opening an empty fuzzy-select.

``` ucm :error
scratch/empty> view

  ⚠️

  Sorry, I was expecting an argument for the definition to view, and I couldn't find any to suggest to you. 😅
```

``` unison :hide
optionOne = 1

nested.optionTwo = 2
```

Definition args

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> debug.fuzzy-options view _

  Select a definition to view:
    * optionOne
    * nested.optionTwo
```

Namespace args

``` ucm
scratch/main> debug.fuzzy-options find-in _

  Select a namespace:
    * nested
```
