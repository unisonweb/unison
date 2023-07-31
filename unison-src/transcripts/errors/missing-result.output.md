
### Transcript parser hidden errors

When an error is encountered in a `unison:hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

```unison
x = 
  y = 24
```



🛑

The transcript failed due to an error in the stanza above. The error is:


  I couldn't find any definitions matching the name _100003 inside the namespace .
  
      2 |   y = 24
  
  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
  
  To add a library to this project use the command: `fork <.path.to.lib> .lib.<libname>`
  
  There are no constraints on its type.
  
  

