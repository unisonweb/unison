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

  ⊙ 1. #h5np977l3l

    + Adds / updates:
    
      x

  ⊙ 2. #sk86d6ag2v

    + Adds / updates:
    
      x

  ⊙ 3. #7i4lvk1322

    + Adds / updates:
    
      x

  ⊙ 4. #tfdfo2rurk

    + Adds / updates:
    
      x

  □ 5. #gjsokvdqtp (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #3ud1dqtnur (start of history)
```
