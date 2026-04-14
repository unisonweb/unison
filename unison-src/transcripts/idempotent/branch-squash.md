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

  ⊙ 1. #al6avfrpnf

    + Adds / updates:
    
      x

  ⊙ 2. #kjcs5q9is4

    + Adds / updates:
    
      x

  ⊙ 3. #0ibemdkfsb

    + Adds / updates:
    
      x

  ⊙ 4. #nrss9684vt

    + Adds / updates:
    
      x

  □ 5. #ivekv79r4p (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #1hnb84fa1d (start of history)
```
