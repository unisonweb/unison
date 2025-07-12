# Integration test: transcript

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
scratch/main> load ./unison-src/transcripts-using-base/base.u
scratch/main> add
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 

  ❓
  
  I couldn't resolve any of these symbols:
  
    384 | Value.transitiveDeps : builtin.Value ->{IO} [(Link.Term, Code)]
      .
    420 | loadValueBytes : Bytes ->{Exception,IO} ([(Link.Term, Code)], builtin.Value)
  
  
  Symbol          Suggestions
                  
  builtin.Value   No matches
```
