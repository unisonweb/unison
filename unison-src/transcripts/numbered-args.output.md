# Using numbered arguments in UCM

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar   : Text
      baz   : Text
      corge : Text
      foo   : Text
      quux  : Text
      qux   : Text

```
```ucm
  ☝️  The namespace .temp is empty.

.temp> add

  ⍟ I've added these definitions:
  
    bar   : Text
    baz   : Text
    corge : Text
    foo   : Text
    quux  : Text
    qux   : Text

```
We can get the list of things in the namespace, and UCM will give us a numbered
list:

```ucm
.temp> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  

```
We can ask to `view` the second element of this list:

```ucm
.temp> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  

.temp> view 2

  baz : Text
  baz = "baz"

```
And we can `view` multiple elements by separating with spaces:

```ucm
.temp> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  

.temp> view 2 3 5

  baz : Text
  baz = "baz"
  
  corge : Text
  corge = "corge"
  
  quux : Text
  quux = "quux"

```
We can also ask for a range:

```ucm
.temp> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  

.temp> view 2-4

  baz : Text
  baz = "baz"
  
  corge : Text
  corge = "corge"
  
  foo : Text
  foo = "foo"

```
And we can ask for multiple ranges and use mix of ranges and numbers:

```ucm
.temp> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  

.temp> view 1-3 4 5-6

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
