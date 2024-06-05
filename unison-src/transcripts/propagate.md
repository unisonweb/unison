# Propagating type edits

```ucm:hide
.subpath.lib> builtins.merge
```

We introduce a type `Foo` with a function dependent `fooToInt`.

```unison
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
```

### Preserving user type variables

We make a term that has a dependency on another term and also a non-redundant
user-provided type signature.

```unison
preserve.someTerm : Optional foo -> Optional foo
preserve.someTerm x = x

preserve.otherTerm : Optional baz -> Optional baz
preserve.otherTerm y = someTerm y
```

Add that to the codebase:

```ucm
.subpath> add
```

Let's now edit the dependency:

```unison
preserve.someTerm : Optional x -> Optional x
preserve.someTerm _ = None
```

Update...

```ucm
.subpath> update
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

```ucm
.subpath> view preserve.someTerm
.subpath> view preserve.otherTerm
```

### Propagation only applies to the local branch

Cleaning up a bit...

```ucm
.> delete.namespace subpath
.subpath.one.lib> builtins.merge
```

Now, we make two terms, where one depends on the other.

```unison
someTerm : Optional foo -> Optional foo
someTerm x = x

otherTerm : Optional baz -> Optional baz
otherTerm y = someTerm y
```

We'll make two copies of this namespace.

```ucm
.subpath.one> add
.subpath> fork one two
```

Now let's edit one of the terms...

```unison
someTerm : Optional x -> Optional x
someTerm _ = None
```

... in one of the namespaces...

```ucm
.subpath.one> update
```

The other namespace should be left alone.

```ucm
.subpath> view two.someTerm
```
