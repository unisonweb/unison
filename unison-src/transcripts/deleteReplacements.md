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

```unison
z = 1
```

```ucm
.> add
.> delete.termReplacement z
.> view.patch
```

```unison
type Foo = Foo
```

```ucm
.> add
```

```unison
type Foo = Foo | Bar
```

```ucm
.> update
.> view.patch
```

```unison
type X = X
```

```ucm
.> add
.> delete.typeReplacement X
.> view.patch
```

