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
scratch/empty> branch.squash scratch/main: squashed

  I squashed scratch/main
   into the hash#59schd8j6uujm52cd163id531p73pabv6jidld02qj9uo36ptlesmoitm5dndj7v7raaavqa70ppa3hifqot4ts3vbnn0cr595rgnf0, but didn't point any branches to it.

scratch/squashed> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #59schd8j6u (start of history)
```

``` ucm
scratch/empty> branch.squash scratch/main: squashed

  I squashed scratch/main
   into scratch/squashed
```

``` ucm
scratch/empty> branch.squash #espq7p1b0h /squashed2

  I squashed #espq7p1b0h
   into the hash#59schd8j6uujm52cd163id531p73pabv6jidld02qj9uo36ptlesmoitm5dndj7v7raaavqa70ppa3hifqot4ts3vbnn0cr595rgnf0, but didn't point any branches to it.

scratch/squashed2> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #59schd8j6u (start of history)
```
