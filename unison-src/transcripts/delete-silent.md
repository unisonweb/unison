```ucm:error
.> delete foo
```

```unison:hide
foo = 1
structural type Foo = Foo ()
```

```ucm
.> add
.> delete foo
.> delete.type Foo
.> delete.term Foo.Foo
```
