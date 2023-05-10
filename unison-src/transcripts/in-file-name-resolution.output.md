Unison should resolve types and terms by suffix even if it's a cyclic reference within the same file.

```unison
structural type deeply.nested.Tree a = 
  Leaf a
  | Node (Tree a) (Tree a)

foo.bar x = bar 1
```

```ucm

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        3 |   | Node (Tree a) (Tree a)
    
    
    Symbol   Suggestions
             
    Tree     No matches
  

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  
    ❓
    
    I couldn't resolve any of these symbols:
    
        3 |   | Node (Tree a) (Tree a)
    
    
    Symbol   Suggestions
             
    Tree     No matches
  

