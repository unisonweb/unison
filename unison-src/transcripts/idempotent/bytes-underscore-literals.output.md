# Underscore separators in bytes literals

Unison supports underscores as visual separators in bytes literals (`0xs...`).
Underscores can appear between hex digits and are stripped before evaluation.

``` ucm :hide
> builtins.merge
```

## Valid literals

Bytes with underscores:

``` unison
> 0xs01_ef
> 0xsAA_BB_CC
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
It looks like this function call:

    1 | > 0xs01_ef


is being applied to 2 arguments, but it has the type

  [Nat] -> Bytes

which only accepts 1 argument.

Maybe you applied the function to too many arguments?

```
