# Propagating type edits

```ucm:hide
.> builtins.merge
```

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
unique type Foo = Foo | Bar
```

and update the codebase to use the new type `Foo`...

```ucm
.subpath> update
```

... it should automatically propagate the type to `fooToInt`.

```ucm
.subpath> view fooToInt
.> cd .
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
.> cd .
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
.> cd .
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

```ucm
.subpath.preserve> view someTerm
.subpath.preserve> view otherTerm
```

### Propagation only applies to the local branch

Cleaning up a bit...

```ucm
.> delete.namespace subpath
```

Now, we make two terms, where one depends on the other.

```unison
use .builtin

someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

We'll make two copies of this namespace.

```ucm
.subpath.one> add
.subpath> fork one two
.> cd .
```

Now let's edit one of the terms...

```unison
use .builtin

someTerm : Optional x -> Optional x
someTerm _ = None
```

... in one of the namespaces...

```ucm
.subpath.one> update
```

The other namespace should be left alone.

```ucm
.subpath.two> view someTerm
```
