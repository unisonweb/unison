Unison does not support Haskell-style pattern matching in function declaration heads.
These tests verify helpful error messages are shown when this is attempted.

## Without a type signature

``` unison :error
isEmpty [] = true
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

    😶
    
    I found a pattern where I expected a variable name or `=`.
    Unison does not support pattern matching in function
    declarations.
    
        1 | isEmpty [] = true
    
    
    Use `case` in the function body instead. For example:
    
        isEmpty arg = case arg of
          ... -> ...
```

## With a type signature

``` unison :error
isEmpty : [a] -> Boolean
isEmpty [] = true
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

    😶
    
    I found a pattern where I expected a variable name or `=`.
    Unison does not support pattern matching in function
    declarations.
    
        2 | isEmpty [] = true
    
    
    Use `case` in the function body instead. For example:
    
        isEmpty arg = case arg of
          ... -> ...
```

## Numeric literal pattern

``` unison :error
factorial 0 = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

    😶
    
    I found a pattern where I expected a variable name or `=`.
    Unison does not support pattern matching in function
    declarations.
    
        1 | factorial 0 = 1
    
    
    Use `case` in the function body instead. For example:
    
        factorial arg = case arg of
          ... -> ...
```
