# Integration test: transcript

```unison
use .builtin

main : '{IO, Exception} ()
main = '(printLine "Hello, world!")
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main : '{IO, Exception} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    main : '{IO, Exception} ()

.> compile main ./unison-cli/integration-tests/IntegrationTests/main

```
