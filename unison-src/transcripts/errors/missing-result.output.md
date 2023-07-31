
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


  The last statement of a block must be an expression, but this is a definition:
  
      2 |   y = 24
  
  I don't know what the result of this block should be.
  Did you forget to add an expression at the end of the block?
  I don't know what type it should be either.

