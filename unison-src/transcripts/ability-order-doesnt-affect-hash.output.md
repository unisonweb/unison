The order of a set of abilities is normalized before hashing.

``` unison
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

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability Bar
      ability Foo
      term1 : '{Bar, Foo} ()
      term2 : '{Bar, Foo} ()

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    ability Bar
    ability Foo
    term1 : '{Bar, Foo} ()
    term2 : '{Bar, Foo} ()

scratch/main> names term1

  Term
  Hash:   #8hum58rlih
  Names:  term1 term2
  
  Tip: Use `names.global` to see more results.

```
