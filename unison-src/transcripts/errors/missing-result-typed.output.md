
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


  The last statement of a block must be an expression, but this is a definition:
  
      3 |   b = 24
  
  I don't know what the result of this block should be.
  Did you forget to add an expression at the end of the block?
  It should be of type Nat.

