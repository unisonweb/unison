# recover-rtti

Recover run-time type information from the GHC heap. The key function in this
library is

```haskell
classify :: a -> Either Closure (Classifier a)
```

which recovers type information about values about which we know nothing (in
particular, no type class constraints). One example use case is the following
`anythingToString` function:

```haskell
anythingToString :: a -> String
```


We test that the result of `anythingToString` is equal to the result of regular
`show` for a large range of data types (including user-defined ones that the
library is not aware of). There are also other possible use cases; for example,
it should be possible to define an `anythingToJSON` function.

Obviously there are limitations; the most important ones are:

* `UNPACK`ed fields are invisible to the library. This does not need to be
  a major issue though, when compiling code with `-O0` fields are not unpacked.
  (https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html?highlight=unpack#nounpack-pragma)
* Record field names are not known, and so records are shown just using the
  constructor.

There may be other gotchas as well; this library is primarily intended
for debugging.
