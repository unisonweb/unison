# `signature` command

`signature` (alias `sig`) displays the type signature of a definition without showing its full body.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

## Basic usage on builtins

``` ucm
scratch/main> signature lib.builtins.List.map
  lib.builtins.List.map
    : (a ->{e} b) -> [a] ->{e} [b]
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️
I don't know how to signature. Type `help` or `?` to get help.
```
