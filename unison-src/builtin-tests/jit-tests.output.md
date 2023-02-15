
Note: This should be forked off of the codebase created by base.md

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

```ucm
.> run.native tests

  💔💥
  
  The program halted with an unhandled exception:
  
    Failure
      (typeLink ANFDecodeError) "unrecognized POp tag" (Any 118)
  
  
  Stack trace:
    ##raise

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  💔💥
  
  The program halted with an unhandled exception:
  
    Failure
      (typeLink ANFDecodeError) "unrecognized POp tag" (Any 118)
  
  
  Stack trace:
    ##raise

