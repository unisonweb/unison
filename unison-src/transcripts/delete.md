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

We want to be able to delete multiple terms at once

```unison:hide
a = "a"
b = "b"
c = "c"
```

```ucm
.> add
.> delete.verbose a b c
```

We can delete terms and types in the same invocation of delete

```unison:hide
structural type Foo = Foo ()
a = "a"
b = "b"
c = "c"
```

```ucm
.> add
.> delete.verbose a b c Foo
.> delete.verbose Foo.Foo
```

We can delete a type and its constructors

```unison:hide
structural type Foo = Foo ()
```

```ucm
.> add
.> delete.verbose Foo Foo.Foo
```

You should not be able to delete terms which are referenced by other terms

```unison:hide
a = 1
b = 2
c = 3
d = a + b + c
```

```ucm:error
.> add
.> delete.verbose a b c
```

But you should be able to delete all terms which reference each other in a single command

```unison:hide
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

```ucm
.> add
.> delete.verbose e f g h
```

You should be able to delete a type and all the functions that reference it in a single command

```unison:hide
structural type Foo = Foo Nat

incrementFoo : Foo -> Nat
incrementFoo = cases
  (Foo n) -> n + 1
```

```ucm
.> add
.> delete.verbose Foo Foo.Foo incrementFoo
```

If you mess up on one of the names of your command, delete short circuits

```unison:hide
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

```ucm:error
.> add
.> delete.verbose e f gg
```

Cyclical terms which are guarded by a lambda are allowed to be deleted

```unison:hide
ping _ = 1 Nat.+ !pong
pong _ = 4 Nat.+ !ping
```

```ucm
.> add
.> delete.verbose ping
.> view pong
```
