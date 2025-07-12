Ensure that Records keep their syntax after being added to the codebase

``` ucm :hide
scratch/main> builtins.merge
scratch/main> load unison-src/transcripts-using-base/base.u
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
