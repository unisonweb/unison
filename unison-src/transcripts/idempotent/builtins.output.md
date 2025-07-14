# Unit tests for builtin functions

``` ucm :hide
scratch/main> builtins.mergeio
scratch/main> load unison-src/transcripts-using-base/base.u
scratch/main> add
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
I couldn't figure out what Value.serialize.versioned refers to here:

  395 |     Some v -> Value.serialize.versioned v

I think its type should be:

    Nat -> Value -> Bytes

Some common causes of this error include:
  * Your current namespace is too deep to contain the definition
    in its subtree
  * The definition is part of a library which hasn't been added
    to this project
  * You have a typo in the name
```
