### Transcript parser operations

``` ucm :hide
> builtins.merge
```

The transcript parser is meant to parse `ucm` and `unison` blocks.

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + x : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison :hide :error scratch.u
z
```

``` ucm :error
> delete foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    foo
```

``` ucm :error
> delete lineToken.call

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    lineToken.call
```

However handling of blocks of other languages should be supported.

``` python
some python code
```

``` c_cpp
some C++ code
```

``` c9search
some cloud9 code
```
