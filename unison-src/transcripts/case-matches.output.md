```unison
structural type Optional a = Some a | None
```

## Common syntax errors should fail to parse

```unison
x = match Some a with
      None -> 
        1
      Some _
        2
```

```ucm

  offset=16:
  unexpected <outdent>
  expecting ,, blank, false, true, or |
      6 | 

```
```unison
x = match Some a with
      None -> 1
           -> 2
           -> 3
```

```ucm

  offset=12:
  unexpected ->
      3 |            -> 2
  

```
Can't have guards following an unguarded expression

```unison
x = match Some a with
      None     -> 1
        | true -> 2
```

```ucm

  offset=12:
  unexpected |
      3 |         | true -> 2
  

```
Must have at least one pattern case.

```unison
x = match Some a with
```

```ucm

    😶
    
    I expected some patterns after a match / with or cases but I
    didn't find any.
    
        1 | x = match Some a with
    

```
