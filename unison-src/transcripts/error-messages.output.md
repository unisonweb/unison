
This file contains programs with parse errors and type errors, for visual inspection of error message quality and to check for regressions or changes to error reporting.

## Parse errors

Some basic errors of literals.

### Floating point literals

```unison
x = 1. -- missing some digits after the decimal
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 1. -- missing some digits after the decimal
  
  I was expecting some digits after the '.', for example: 1.0 or
  1.1e37.

```
```unison
x = 1e -- missing an exponent
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 1e -- missing an exponent
  
  I was expecting some digits for the exponent, for example:
  1e37.

```
```unison
x = 1e- -- missing an exponent
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 1e- -- missing an exponent
  
  I was expecting some digits for the exponent, for example:
  1e-37.

```
```unison
x = 1E+ -- missing an exponent
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 1E+ -- missing an exponent
  
  I was expecting some digits for the exponent, for example:
  1e+37.

```
### Hex, octal, and bytes literals

```unison
x = 0xoogabooga -- invalid hex chars
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 0xoogabooga -- invalid hex chars
  
  I was expecting only hexidecimal characters (one of
  0123456789abcdefABCDEF) after the 0x.

```
```unison
x = 0o987654321 -- 9 and 8 are not valid octal char
```

```ucm

  This number isn't valid syntax: 
  
      1 | x = 0o987654321 -- 9 and 8 are not valid octal char
  
  I was expecting only octal characters (one of 01234567) after
  the 0o.

```
```unison
x = 0xsf -- odd number of hex chars in a bytes literal
```

```ucm

  This bytes literal isn't valid syntax: 0xsf
  
      1 | x = 0xsf -- odd number of hex chars in a bytes literal
  
  I was expecting an even number of hexidecimal characters (one
  of 0123456789abcdefABCDEF) after the 0xs.

```
```unison
x = 0xsnotvalidhexchars -- invalid hex chars in a bytes literal
```

```ucm

  This bytes literal isn't valid syntax: 0xsnotvalidhexchars
  
      1 | x = 0xsnotvalidhexchars -- invalid hex chars in a bytes literal
  
  I was expecting an even number of hexidecimal characters (one
  of 0123456789abcdefABCDEF) after the 0xs.

```
### Layout errors

```unison
foo = else -- not matching if
```

```ucm

  I found a closing 'else' here without a matching 'then'.
  
      1 | foo = else -- not matching if
  

```
```unison
foo = then -- unclosed
```

```ucm

  I found a closing 'then' here without a matching 'if'.
  
      1 | foo = then -- unclosed
  

```
```unison
foo = with -- unclosed
```

```ucm

  I found a closing 'with' here without a matching 'handle' or 'match'.
  
      1 | foo = with -- unclosed
  

```
```unison
foo = match 1 with
  2 -- no right-hand-side
```

```ucm

    😶
    
    I expected some patterns after a match / with but I didn't
    find any.
        1 | foo = match 1 with
    

```
### Keywords

```unison
use.keyword.in.namespace = 1
```

```ucm

  The identifier used here isn't allowed to be a reserved keyword: 
  
      1 | use.keyword.in.namespace = 1
  

```
```unison
-- reserved operator
a ! b = 1
```

```ucm

  This looks like the start of an expression here 
  
      2 | a ! b = 1
  
  but at the file top-level, I expect one of the following:
  
    - A binding, like a = 42 OR
                      a : Nat
                      a = 42
    - A watch expression, like > a + 1
    - An `ability` declaration, like ability Foo where ...
    - A `type` declaration, like type Optional a = None | Some a
    - A `namespace` declaration, like namespace Seq where ...
  

```
