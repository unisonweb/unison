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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique ability Bar
      unique ability Foo
      term1 : '{Foo, Bar} ()
      term2 : '{Foo, Bar} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique ability Bar
    unique ability Foo
    term1 : '{Foo, Bar} ()
    term2 : '{Foo, Bar} ()

.> names term1

  Term
  Hash:   #3g90c9l64s
  Names:  term1 term2

```
