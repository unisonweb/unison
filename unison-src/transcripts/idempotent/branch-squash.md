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

  ⊙ 1. #qsv0hna5g5

    + Adds / updates:
    
      x

  ⊙ 2. #u5mjc4c1c2

    + Adds / updates:
    
      x

  ⊙ 3. #980f65ejq3

    + Adds / updates:
    
      x

  ⊙ 4. #s88sba9fg5

    + Adds / updates:
    
      x

  □ 5. #18am4hkn5e (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #8pthloms9s (start of history)
```
