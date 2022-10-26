
```ucm:hide
.> builtins.merge
```

This file contains programs with parse errors and type errors, for visual inspection of error message quality and to check for regressions or changes to error reporting.

## Parse errors

Some basic errors of literals.

### Floating point literals

```unison:error
x = 1. -- missing some digits after the decimal
```

```unison:error
x = 1e -- missing an exponent
```

```unison:error
x = 1e- -- missing an exponent
```

```unison:error
x = 1E+ -- missing an exponent
```

### Hex, octal, and bytes literals

```unison:error
x = 0xoogabooga -- invalid hex chars
```

```unison:error
x = 0o987654321 -- 9 and 8 are not valid octal char
```

```unison:error
x = 0xsf -- odd number of hex chars in a bytes literal
```

```unison:error
x = 0xsnotvalidhexchars -- invalid hex chars in a bytes literal
```

### Layout errors

```unison:error
foo = else -- not matching if
```

```unison:error
foo = then -- unclosed
```

```unison:error
foo = with -- unclosed
```

### Matching

```unison:error
foo = match 1 with
  2 -- no right-hand-side
```

```unison:error
-- Mismatched arities
foo = cases
  1, 2 -> ()
  3 -> ()
```


### Watches

```unison:error
-- Empty watch
>
```

### Keywords

```unison:error
use.keyword.in.namespace = 1
```

```unison:error
-- reserved operator
a ! b = 1
```
