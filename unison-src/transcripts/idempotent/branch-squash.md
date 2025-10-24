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

  ⊙ 1. #gr79soqkbt

    + Adds / updates:
    
      x

  ⊙ 2. #0bkum24eja

    + Adds / updates:
    
      x

  ⊙ 3. #ih62ljkid2

    + Adds / updates:
    
      x

  ⊙ 4. #mg97f9rgus

    + Adds / updates:
    
      x

  □ 5. #nsv9863lsg (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #7l8c1b4av8 (start of history)
```
