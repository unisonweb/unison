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
.> delete.term-replacement #jk19
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

```ucm
.> delete.type-replacement #568rsi7o3g
.> view.patch
```

```unison
bar = 3
type bar = Foo
```

```ucm
.> add
```

```unison
type bar = Foo | Bar
```

```ucm
.> update
.> view.patch
.> delete.type-replacement bar
.> view.patch
```
