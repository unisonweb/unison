
### Transcript parser hidden errors

When an error is encountered in a `unison:hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

```unison
a : ##Nat
a = 
  b = 24
```



🛑

The transcript failed due to an error in the stanza above. The error is:


  I couldn't find a type for ##Nat.
  
      1 | a : ##Nat
  
  Make sure it's spelled correctly.

