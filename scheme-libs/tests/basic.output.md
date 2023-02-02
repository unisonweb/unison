
Note: This should be forked off of the codebase created by base.md

```unison
test1_term = '(printLine "Hello")
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1_term : '{IO, Exception} ()

```
```unison
test1 = '(runInScheme 1 (termLink test1_term))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1 : '{IO, Exception} Text

```
```ucm
.> run test1

  "Hello\n"

```
