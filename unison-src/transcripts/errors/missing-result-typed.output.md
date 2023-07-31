
### Transcript parser hidden errors

When an error is encountered in a `unison:hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

```unison
a : Nat
a = 
  b = 24
```



🛑

The transcript failed due to an error in the stanza above. The error is:


  I couldn't find any definitions matching the name _150003 inside the namespace .
  
      3 |   b = 24
  
  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
  
  To add a library to this project use the command: `fork <.path.to.lib> .lib.<libname>`
  
  Whatever it is, its type should conform to Nat.
  
  

