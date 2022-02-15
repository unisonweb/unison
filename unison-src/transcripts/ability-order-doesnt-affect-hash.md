The order of a set of abilities is normalized before hashing.

```unison
unique ability Foo where
  foo : ()

unique ability Bar where
  bar : ()

term1 : () ->{Foo, Bar} ()
term1 _ = ()

term2 : () ->{Bar, Foo} ()
term2 _ = ()
```

```ucm
.> add
.> names term1
```
