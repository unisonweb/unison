# Test that the options selector for fuzzy finding is working as expected for different argument types.


```unison
optionOne = 1

nested.optionTwo = 2
```

Definition args

```ucm
.> add
.> debug.fuzzy-options view _
```


Namespace args

```ucm
.> add
.> debug.fuzzy-options cd _
```

Project Branch args

```ucm
.> project.create-empty myproject
myproject/main> branch mybranch
.> debug.fuzzy-options switch _
```

