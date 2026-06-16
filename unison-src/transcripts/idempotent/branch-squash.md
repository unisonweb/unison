# `branch.squash`

``` ucm :hide
scratch/main> builtins.merge
```

Build up some history:

``` unison :hide
x = 1
```

``` ucm :hide
scratch/main> add
```

``` unison :hide
x = 2
```

``` ucm :hide
scratch/main> update
```

``` unison :hide
x = 3
```

``` ucm :hide
scratch/main> update
```

``` unison :hide
x = 4
```

``` ucm :hide
scratch/main> update
```

`branch.squash` with both a source and destination.

``` ucm
scratch/empty> branch.squash scratch/main /squashed

  I squashed scratch/main into scratch/squashed

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #55m323nbut

    + Adds / updates:
    
      x

  ⊙ 2. #931n3djbei

    + Adds / updates:
    
      x

  ⊙ 3. #mjih6n9917

    + Adds / updates:
    
      x

  ⊙ 4. #od6s60qe8a

    + Adds / updates:
    
      x

  □ 5. #5cka318i78 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #vj5gm9g1q8 (start of history)
```
