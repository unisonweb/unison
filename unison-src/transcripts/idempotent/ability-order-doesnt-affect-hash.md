The order of a set of abilities is normalized before hashing.

``` unison :hide
unique ability Foo where
  foo : ()

unique ability Bar where
  bar : ()

term1 : () ->{Foo, Bar} ()
term1 _ = ()

term2 : () ->{Bar, Foo} ()
term2 _ = ()
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> names term1

  'term1':
  Hash          Kind   Names
  #42m1ui9g56   Term   term1, term2
```
