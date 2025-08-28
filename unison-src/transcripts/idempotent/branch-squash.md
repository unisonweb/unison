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

  ⊙ 1. #rklc0afal3

    + Adds / updates:
    
      x

  ⊙ 2. #ddbj1dn74u

    + Adds / updates:
    
      x

  ⊙ 3. #obi1rtc0qt

    + Adds / updates:
    
      x

  ⊙ 4. #jqcff0fc3h

    + Adds / updates:
    
      x

  □ 5. #3h5ppoqiv3 (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #bg4r7g98r1 (start of history)
```
