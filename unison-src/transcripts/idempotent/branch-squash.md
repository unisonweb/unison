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

  ⊙ 1. #gqp66rpf7q

    + Adds / updates:
    
      x

  ⊙ 2. #f01ltsh573

    + Adds / updates:
    
      x

  ⊙ 3. #65i3vdlh8n

    + Adds / updates:
    
      x

  ⊙ 4. #41811gepfk

    + Adds / updates:
    
      x

  □ 5. #i8k794a3i9 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #8h52r1bjik (start of history)
```
