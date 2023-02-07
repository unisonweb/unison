
Note: This should be forked off of the codebase created by base.md

```ucm
.> load unison-src/builtin-tests/testlib.u

  I found and typechecked these definitions in
  unison-src/builtin-tests/testlib.u. If you do an `add` or
  `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique ability Tests
      Tests.check      : Text
                         -> '{g, Exception} Boolean
                         ->{g, Tests} ()
      Tests.checkEqual : Text -> a1 -> a1 ->{Tests} ()
      Tests.main       : '{IO, Exception, Tests} ()
                         -> '{IO, Exception} ()
      Tests.run        : '{IO, Exception, Tests} ()
                         ->{IO, Exception} Boolean

.> add

  ⍟ I've added these definitions:
  
    unique ability Tests
    Tests.check      : Text
                       -> '{g, Exception} Boolean
                       ->{g, Tests} ()
    Tests.checkEqual : Text -> a1 -> a1 ->{Tests} ()
    Tests.main       : '{IO, Exception, Tests} ()
                       -> '{IO, Exception} ()
    Tests.run        : '{IO, Exception, Tests} ()
                       ->{IO, Exception} Boolean

.> load unison-src/builtin-tests/tests.u

  I found and typechecked these definitions in
  unison-src/builtin-tests/tests.u. If you do an `add` or
  `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      tests : '{IO, Exception} ()

.> add

  ⍟ I've added these definitions:
  
    tests : '{IO, Exception} ()

```
```ucm
.> run tests

  ()

```
