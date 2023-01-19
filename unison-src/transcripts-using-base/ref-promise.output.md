Script to test the basic functionality of CAS and Promise.

Ref support a CAS operation that can be used as a building block to
change state atomically without locks.

```unison
casTest: '{io2.IO} [Result]
casTest _ =
  test _ =
    ref = IO.ref 0
    ticket = Ref.readForCas ref
    v1 = Ref.cas ref ticket 5
    check "CAS is successful is there were no conflicting writes" v1
    Ref.write ref 10
    v2 = Ref.cas ref ticket 15
    check "CAS fails when there was an intervening write" v2
   
  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      casTest : '{IO} [Result]

```
