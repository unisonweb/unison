# Test that the options selector for fuzzy finding is working as expected for different argument types.

If an argument is required but doesn't have a fuzzy resolver, the command should just print the help.


```ucm:error
-- The second argument of move.term is a 'new-name' and doesn't have a fuzzy resolver
.> move.term
```

If a fuzzy resolver doesn't have any options available it should print a message instead of
opening an empty fuzzy-select.

```ucm:error
.empty> view
```


```unison:hide
optionOne = 1

nested.optionTwo = 2

lib.dep.defnInLib = 3

lib.dep.lib.tdep.defnInTransitiveDep = 4
```

Definition args

```ucm
.> add
.> debug.fuzzy-options view _
.> debug.fuzzy-options edit _
```

Namespace args

```ucm
.> debug.fuzzy-options find-in _
```

Project Branch args

```ucm
.> project.create-empty myproject
myproject/main> branch mybranch
.> debug.fuzzy-options switch _
```
