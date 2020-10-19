# Using numbered arguments in UCM

```ucm:hide
.> builtins.merge
```

First lets add some contents to our codebase.

```unison
foo = "foo"
bar = "bar"
baz = "baz"
qux = "qux"
quux = "quux"
corge = "corge"
```

```ucm
.temp> add
```

We can get the list of things in the namespace, and UCM will give us a numbered
list:

```ucm
.temp> find
```

We can ask to `view` the second element of this list:

```ucm
.temp> find
.temp> view 2
```

And we can `view` multiple elements by separating with spaces:

```ucm
.temp> find
.temp> view 2 3 5
```

We can also ask for a range:

```ucm
.temp> find
.temp> view 2-4
```

And we can ask for multiple ranges and use mix of ranges and numbers:

```ucm
.temp> find
.temp> view 1-3 4 5-6
```

