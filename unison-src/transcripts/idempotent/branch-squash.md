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

  ⊙ 1. #taer69m8qa

    + Adds / updates:
    
      x

  ⊙ 2. #0fbhrl5dr6

    + Adds / updates:
    
      x

  ⊙ 3. #jgmtkf36k8

    + Adds / updates:
    
      x

  ⊙ 4. #iqjicvfjug

    + Adds / updates:
    
      x

  □ 5. #fqdkg8np69 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #2suqcpr38s (start of history)
```
