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
