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

  ⊙ 1. #5750c8ofa3

    + Adds / updates:
    
      x

  ⊙ 2. #5l51lg1tk6

    + Adds / updates:
    
      x

  ⊙ 3. #6fhf9ragce

    + Adds / updates:
    
      x

  ⊙ 4. #gg9qvgu4en

    + Adds / updates:
    
      x

  □ 5. #44c672ibnd (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #2v9p6que0u (start of history)
```
