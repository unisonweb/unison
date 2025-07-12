# Unit tests for builtin functions

``` ucm :hide
scratch/main> builtins.mergeio
scratch/main> load unison-src/transcripts-using-base/base.u
scratch/main> add
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 

  ❓
  
  I couldn't resolve any of these symbols:
  
    384 | Value.transitiveDeps : builtins.Value ->{IO} [(Link.Term, Code)]
      .
    420 | loadValueBytes : Bytes ->{Exception,IO} ([(Link.Term, Code)], builtins.Value)
  
  
  Symbol           Suggestions
                   
  builtins.Value   No matches
```
