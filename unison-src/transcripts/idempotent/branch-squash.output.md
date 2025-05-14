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
scratch/empty> history /squashed
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️

Sorry, I wasn’t sure how to process your request:

  1:2:
    |
  1 | /squashed
    |  ^
  unexpected 's'
  expecting '.', end of input, or operator (valid characters: !$%&*+-/:<=>\^|~)
  

You can run `help history` for more information on using
`history`.
```
