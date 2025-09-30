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

  ⊙ 1. #s9t33tj214

    + Adds / updates:
    
      x

  ⊙ 2. #bot67o78do

    + Adds / updates:
    
      x

  ⊙ 3. #1ra4g2toic

    + Adds / updates:
    
      x

  ⊙ 4. #rpktkqb6c9

    + Adds / updates:
    
      x

  □ 5. #vcqarvnndj (start of history)

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #tcocmb66da (start of history)
```
