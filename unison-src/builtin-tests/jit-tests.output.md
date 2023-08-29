
Note: This should be forked off of the codebase created by base.md

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

```ucm
.> alias.term ##IO.randomBytes IO.randomBytes.> load unison-src/builtin-tests/io-tests.u.> add
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  The 1st argument to `(Int.>)`
  
            has type:  Nat
      but I expected:  Int
  
     47 |             if seconds Int.> +10 then
  

