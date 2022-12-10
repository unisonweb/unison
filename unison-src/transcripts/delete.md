# Delete

```ucm:hide
.> builtins.merge
```

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm:error
.> delete.verbose foo
```

Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison:hide
foo = 1
structural type Foo = Foo ()
```

```ucm
.> add
.> delete.verbose foo
.> delete.verbose Foo
.> delete.verbose Foo.Foo
```

How about an ambiguous term?

```unison:hide
foo = 1
```

```ucm
.a> add
```

```unison:hide
foo = 2
```

```ucm
.b> add
.a> merge .b
```

A delete should remove both versions of the term.

```ucm
.a> delete.verbose foo
```

```ucm:error
.a> ls
```

Let's repeat all that on a type, for completeness.

```unison:hide
structural type Foo = Foo ()
```

```ucm
.a> add
```

```unison:hide
structural type Foo = Foo
```

```ucm
.b> add
.a> merge .b
```

```ucm
.a> delete.verbose Foo
```

```ucm
.a> delete.verbose Foo.Foo
```

Finally, let's try to delete a term and a type with the same name.

```unison:hide
foo = 1
structural type foo = Foo ()
```

```ucm
.> add
```

```ucm
.> delete.verbose foo
```
