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

  ⊙ 1. #cj9gvcnaur

    + Adds / updates:
    
      x

  ⊙ 2. #it68rpvb6q

    + Adds / updates:
    
      x

  ⊙ 3. #or8uepvfr6

    + Adds / updates:
    
      x

  ⊙ 4. #vjj2uk50d5

    + Adds / updates:
    
      x

  □ 5. #esoicgueki (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #fjn2o87986 (start of history)
```
