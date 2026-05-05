# Pattern matching on Bytes literals

Bytes literals (`0xs...`) can be used as patterns, just like `Text`, `Nat`,
`Int`, `Float`, `Char`, and `Boolean` literals.

``` ucm :hide
scratch/main> builtins.mergeio
```

## Basic Bytes literal patterns

``` unison
classify : Bytes -> Text
classify = cases
  0xs0102 -> "one-two"
  0xsabcd -> "abcd"
  0xs     -> "empty"
  _       -> "other"

> classify 0xs0102
> classify 0xsabcd
> classify 0xs
> classify 0xsff
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
I got confused here:

    3 |   0xs0102 -> "one-two"


I was surprised to find a bytes literal here.
I was expecting one of these instead:

* end of input
* pattern
```
