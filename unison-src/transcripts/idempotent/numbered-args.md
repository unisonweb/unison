# Using numbered arguments in UCM

``` ucm :hide
> alias.type ##Text Text
```

First lets add some contents to our codebase.

``` unison
foo = "foo"
bar = "bar"
baz = "baz"
qux = "qux"
quux = "quux"
corge = "corge"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar   : Text
  + baz   : Text
  + corge : Text
  + foo   : Text
  + quux  : Text
  + qux   : Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

We can get the list of things in the namespace, and UCM will give us a numbered
list:

``` ucm
> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text
```

We can ask to `view` the second element of this list:

``` ucm
> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

> view 2

  baz : Text
  baz = "baz"
```

And we can `view` multiple elements by separating with spaces:

``` ucm
> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

> view 2 3 5

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  quux : Text
  quux = "quux"
```

We can also ask for a range:

``` ucm
> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

> view 2-4

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  foo : Text
  foo = "foo"
```

And we can ask for multiple ranges and use mix of ranges and numbers:

``` ucm
> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

> view 1-3 4 5-6

  bar : Text
  bar = "bar"

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  foo : Text
  foo = "foo"

  quux : Text
  quux = "quux"

  qux : Text
  qux = "qux"
```
