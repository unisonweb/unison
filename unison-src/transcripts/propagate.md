# Propagating type edits

```ucm:hide
scratch/main> builtins.merge lib.builtins
```

We introduce a type `Foo` with a function dependent `fooToInt`.

```unison
unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

And then we add it.

```ucm
scratch/main> add
scratch/main> find.verbose
scratch/main> view fooToInt
```

Then if we change the type `Foo`...

```unison
unique type Foo = Foo | Bar
```

and update the codebase to use the new type `Foo`...

```ucm
scratch/main> update.old
```

... it should automatically propagate the type to `fooToInt`.

```ucm
scratch/main> view fooToInt
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
scratch/main> add
```

Let's now edit the dependency:

```unison
preserve.someTerm : Optional x -> Optional x
preserve.someTerm _ = None
```

Update...

```ucm
scratch/main> update.old
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

```ucm
scratch/main> view preserve.someTerm
scratch/main> view preserve.otherTerm
```

### Propagation only applies to the local branch

Cleaning up a bit...

```ucm
.subpath.lib> builtins.merge
```

Now, we make two terms, where one depends on the other.

```unison
one.someTerm : Optional foo -> Optional foo
one.someTerm x = x

one.otherTerm : Optional baz -> Optional baz
one.otherTerm y = someTerm y
```

We'll make two copies of this namespace.

```ucm
.subpath> add
.subpath> fork one two
```

Now let's edit one of the terms...

```unison
someTerm : Optional x -> Optional x
someTerm _ = None
```

... in one of the namespaces...

```ucm
.subpath.one> update.old
```

The other namespace should be left alone.

```ucm
.subpath> view two.someTerm
```
