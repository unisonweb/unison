``` ucm :error
scratch/main> delete foo
```

``` unison :hide
foo = 1
structural type Foo = Foo ()
```

``` ucm
scratch/main> add
scratch/main> delete foo
scratch/main> delete.type Foo
scratch/main> delete.term Foo.Foo
```
