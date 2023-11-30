# Linking definitions to metadata

The `link` and `unlink` commands can be used to manage metadata linked to definitions. For example, you can link documentation to a definition:

```unison
use .builtin

coolFunction x = x * 2

coolFunction.doc = [: This is a cool function. :]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      coolFunction     : Nat -> Nat
      coolFunction.doc : Doc

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    coolFunction     : Nat -> Nat
    coolFunction.doc : Doc

.> link coolFunction.doc coolFunction

```



🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to link. Type `help` or `?` to get help.
