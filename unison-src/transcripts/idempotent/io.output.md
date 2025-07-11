# tests for built-in IO functions

``` ucm :hide
scratch/main> builtins.merge
scratch/main> builtins.mergeio
scratch/main> load unison-src/transcripts-using-base/base.u
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 

  ❓
  
  I couldn't resolve any of these symbols:
  
    384 | Value.transitiveDeps : Value ->{IO} [(Link.Term, Code)]
      .
    420 | loadValueBytes : Bytes ->{Exception,IO} ([(Link.Term, Code)], Value)
  
  
  Symbol   Suggestions
           
  Value    builtin.Value
           builtin.avro.Value
```
