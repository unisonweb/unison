```ucm
.> delete foo

  ⚠️
  
  I don't know about those names.

```
```unison
foo = 1
structural type Foo = Foo ()
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    foo : ##Nat

.> delete foo

  Done.

.> delete.type Foo

  Done.

.> delete.term Foo.Foo

  Done.

```
