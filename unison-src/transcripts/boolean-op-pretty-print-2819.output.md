Regression test for https://github.com/unisonweb/unison/pull/2819

```unison
hangExample : Boolean
hangExample =
  ("a long piece of text to hang the line" == "")
    && ("a long piece of text to hang the line" == "")
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      hangExample : Boolean

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    hangExample : Boolean

.> view hangExample

  hangExample : Boolean
  hangExample =
    use Text ==
    ("a long piece of text to hang the line" == "")
      && ("a long piece of text to hang the line" == "")

```
