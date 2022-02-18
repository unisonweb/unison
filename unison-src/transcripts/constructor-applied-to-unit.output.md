```unison
structural type Zoink a b c = Zoink a b c

> Any ()
> [ Zoink [0,1,2,3,4,5] [6,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,3] () ]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Zoink a b c
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    3 | > Any ()
          ⧩
          Any ()
  
    4 | > [ Zoink [0,1,2,3,4,5] [6,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,3] () ]
          ⧩
          [ Zoink
              [0, 1, 2, 3, 4, 5]
              [ 6,
                3,
                3,
                3,
                3,
                3,
                3,
                3,
                3,
                3,
                3,
                4,
                4,
                4,
                4,
                4,
                4,
                4,
                4,
                4,
                3 ]
              () ]

```
