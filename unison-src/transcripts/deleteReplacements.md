# Deleting term and type replacements from patches

```unison
x = 1
```

```ucm
.> add
```

```unison
x = 2
```

```ucm
.> update
.> view.patch
```

```ucm
.> delete.term-replacement 1
.> view.patch
```

```unison
unique[a] type Foo = Foo
```

```ucm
.> add
```

```unison
unique[b] type Foo = Foo | Bar
```

```ucm
.> update
.> view.patch
```

```ucm
.> delete.type-replacement 1
.> view.patch
```

```unison
bar = 3
unique[aa] type bar = Foo
```

```ucm
.> add
```

```unison
unique[bb] type bar = Foo | Bar
```

```ucm
.> update
.> view.patch
.> delete.type-replacement 1
.> view.patch
```

we get an error when attempting to delete something that is neither a type nor a term
```ucm:error
.> view.patch
.> delete.type-replacement not-here
.> view.patch
```

When attempting to delete a type/term that doesn't exist, but a term/type exists
with that name, alert the user.
```unison
baz = 0
```

```ucm:error
.> add baz
.> delete.type-replacement baz
.> view.patch
```

```unison
unique type qux = Qux
```

```ucm:error
.> add qux
.> delete.term-replacement qux
.> view.patch
```
