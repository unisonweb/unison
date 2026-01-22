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

  ⊙ 1. #qicriqne7g

    + Adds / updates:
    
      x

  ⊙ 2. #qnrdsm6d02

    + Adds / updates:
    
      x

  ⊙ 3. #s3hb8u7j6a

    + Adds / updates:
    
      x

  ⊙ 4. #5c39bhle78

    + Adds / updates:
    
      x

  □ 5. #60gjtmgu82 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #ojehj95jd3 (start of history)
```
