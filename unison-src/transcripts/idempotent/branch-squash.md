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

  ⊙ 1. #bngq90liom

    + Adds / updates:
    
      x

  ⊙ 2. #ibj9ucfel7

    + Adds / updates:
    
      x

  ⊙ 3. #a4a7kv27iq

    + Adds / updates:
    
      x

  ⊙ 4. #7908j4baqi

    + Adds / updates:
    
      x

  □ 5. #llkks335gk (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #lg2ckd5shf (start of history)
```
