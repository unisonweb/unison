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

  ⊙ 1. #5g5f3u7ndc

    + Adds / updates:
    
      x

  ⊙ 2. #voc2smkl6i

    + Adds / updates:
    
      x

  ⊙ 3. #mm3eg0qds0

    + Adds / updates:
    
      x

  ⊙ 4. #uu2kvr6fuu

    + Adds / updates:
    
      x

  □ 5. #mc3dil3epj (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #a12k283p1j (start of history)
```
