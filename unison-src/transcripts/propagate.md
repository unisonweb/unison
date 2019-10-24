# Propagating type edits

We introduce a type `Foo` with a function dependent `fooToInt`.

```unison
use .builtin

unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

And then we add it.

```ucm
.subpath> add
.subpath> find.verbose
.subpath> view fooToInt
```

Then if we change the type `Foo`...

```unison
type Foo = Foo | Bar
```

and update the codebase to use the new type `Foo`...

```ucm
.subpath> update
```

... it should automatically propagate the type to `fooToInt`.

```ucm
.subpath> view fooToInt
```

### Preserving user type variables

We make a term that has a dependency on another term and also a non-redundant
user-provided type signature.

```unison
use .builtin

someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

Add that to the codebase:

```ucm
.subpath.preserve> add
```

Let's now edit the dependency:

```unison
use .builtin

someTerm : Optional x -> Optional x
someTerm _ = None
```

Update...

```ucm
.subpath.preserve> update
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the 
type of `otherTerm` should remain the same.

```ucm
.subpath.preserve> view someTerm
.subpath.preserve> view otherTerm
```

### Propagation only applies to the local branch

```ucm
.> delete.namespace subpath
```

```unison
use .builtin

someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

```ucm
.subpath.one> add
.subpath> fork one two
```

```unison
use .builtin

someTerm : Optional x -> Optional x
someTerm _ = None
```

```ucm
.subpath.one> update
```

```ucm
.subpath.two> view someTerm
```

